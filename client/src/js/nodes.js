/*
 * This is a service worker script which sw-precache will import.
 *
 * This gets 'included' in service-worker.js via an `importScripts`.
 * Note that it runs **in that context**, so we create a separate
 * context here with an immediately-executing function.
 */
'use strict';

// This defines endpoints that the Elm app can call via HTTP requests
// in order to get data from IndexedDB. The goal is to make this look,
// to Elm, very similar to getting data from a Drupal backend. We'll
// start by implementing just the things we need -- over time, it may
// become more comprehensive.

(async () => {

    // As we defined Dexie's store in app.js, we'll need to redefine tables properties here.
    // Since we don't know exactly when the DB will be ready, we define DB placeholder here.
    var dbSync = null;
    var db = null;

    // This defines our URL scheme. A URL will look like
    //
    // /sw/nodes/health_center
    //
    // ... where '/sw/nodes/' is a prefix, and then the next part is
    // the 'type' of the bundle we're looking for. Then, if we're addressing
    // a specific one, then we'll have its UUID at the end. So, you might have:
    //
    // /sw/nodes/health_center/78cf21d1-b3f4-496a-b312-d8ae73041f09
    var nodesUrlRegex = /\/sw\/nodes\/([^/]+)\/?(.*)/;

    self.addEventListener('fetch', async function (event) {
        // If placeholder still indicates tha DB was not initialized.
        if (dbSync === null) {
            // Check if IndexedDB exists.
            var dbExists = await Dexie.exists('sync');
            if (!dbExists) {
              // Skip any further actions, if it's not.
              return;
            }

            // Redefine tables properties.
            dbSync = new Dexie('sync');
            db = await dbSync.open();
            db.tables.forEach(function(table) {
                dbSync[table.name] = table;
            });

            dbSync.shards.hook('creating', function (primKey, obj, trans) {
              if (obj.type === 'person') {
                if (typeof obj.label == 'string') {
                  obj.name_search = gatherWords(obj.label);
                }
              }
            });

            dbSync.shards.hook('updating', function (mods, primKey, obj, trans) {
              if (obj.type === 'person') {
                if (mods.hasOwnProperty("label")) {
                  if (typeof mods.label == 'string') {
                    return {
                      name_search: gatherWords(mods.label)
                    };
                  } else {
                    return {
                      name_search: []
                    };
                  }
                } else {
                  return {
                    name_search: gatherWords(obj.label)
                  };
                }
              }
            });

            // A hook to create matching row in `authorityPhotoUploadChanges`, if entity has a photo.
            dbSync.shardChanges.hook('creating', function (primKey, obj, transaction) {
                const self = this;
                return addPhotoUploadChanges(self, dbSync.authorityPhotoUploadChanges, obj);
            });
        }

        var url = new URL(event.request.url);
        var matches = nodesUrlRegex.exec(url.pathname);

        if (matches) {
            var type = matches[1];
            var uuid = matches[2]; // May be null

            if (event.request.method === 'GET') {
                if (uuid) {
                    if (type === 'child-measurements' || type === 'mother-measurements') {
                        return event.respondWith(viewMeasurements('person', uuid));
                    }
                    else if (type === 'prenatal-measurements') {
                        return event.respondWith(viewMeasurements('prenatal_encounter', uuid));
                    }
                    else if (type === 'nutrition-measurements') {
                        return event.respondWith(viewMeasurements('nutrition_encounter', uuid));
                    }
                    else if (type === 'acute-illness-measurements') {
                        return event.respondWith(viewMeasurements('acute_illness_encounter', uuid));
                    }
                    else {
                        return event.respondWith(view(type, uuid));
                    }
                } else {
                      return event.respondWith(index(url, type));
                }
            }

            if (event.request.method === 'DELETE') {
                if (uuid) {
                    return event.respondWith(deleteNode(url, type, uuid));
                }
            }

            if (event.request.method === 'PUT') {
                if (uuid) {
                    return event.respondWith(putNode(event.request, type, uuid));
                }
            }

            if (event.request.method === 'POST') {
                return event.respondWith(postNode(event.request, type));
            }

            if (event.request.method === 'PATCH') {
                if (uuid) {
                    return event.respondWith(patchNode(event.request, type, uuid));
                }
            }

            // If we get here, respond with a 404
            var response = new Response('', {
                status: 404,
                statusText: 'Not Found'
            });

            return event.respondWith(response);

        }
    });

    var Status = {
        published: 1,
        unpublished: 0
    };

    function expectedOnDate (participation, sessionDate) {
        var joinedGroupBeforeSession = participation.expected.value <= sessionDate;
        var notLeftGroup = !participation.expected.value2 || participation.expected.value2 > sessionDate;

        return joinedGroupBeforeSession && notLeftGroup;
    }

    function getTableForType (type) {
        var table = tableForType[type];

        if (table) {
            return Promise.resolve(dbSync[table]);
        } else {
            var response = new Response('', {
                status: 404,
                statusText: 'Type ' + type + ' not found'
            });

            return Promise.reject(response);
        }
    }

    function deleteNode (url, type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {
            if (type === 'syncmetadata') {
                // For the syncmetadata type, we actually delete
                return dbSync.syncMetadata.delete(uuid).catch(databaseError).then(sendSyncData).then(function () {
                    var response = new Response(null, {
                        status: 204,
                        statusText: 'Deleted'
                    });

                    return Promise.resolve(response);
                });
            } else {
                // Otherwise, we set the status to unpublished
                return getTableForType(type).then(function (table) {
                    return table.update(uuid, {status: Status.unpublished}).catch(databaseError).then(function (updated) {
                        var response = new Response(null, {
                            status: 204,
                            statusText: 'Deleted'
                        });

                        return Promise.resolve(response);
                    });
                });
            }
        }).catch(sendErrorResponses);
    }

    function putNode (request, type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {
            return getTableForType(type).then(function (table) {
                return request.json().catch(jsonError).then(function (json) {
                    json.uuid = uuid;
                    json.type = type;

                    return table.put(json).catch(databaseError).then(function () {
                        return sendRevisedNode(table, uuid).then(function () {
                            // For hooks to be able to work, we need to declare the
                            // tables that may be altered due to a change. In this case
                            // We want to allow adding pending upload photos.
                            // See for example dbSync.nodeChanges.hook().
                            return db.transaction('rw', table, dbSync.nodeChanges, dbSync.shardChanges,  dbSync.authorityPhotoUploadChanges, function() {
                                var body = JSON.stringify({
                                    data: [json]
                                });

                                var response = new Response(body, {
                                    status: 200,
                                    statusText: 'OK',
                                    headers: {
                                        'Content-Type': 'application/json'
                                    }
                                });

                                return Promise.resolve(response);
                            });
                        });
                    });
                });
            });
        }).catch(sendErrorResponses);
    }

    function patchNode (request, type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {
            return getTableForType(type).then(function (table) {
                return request.json().catch(jsonError).then(function (json) {
                    return table.update(uuid, json).catch(databaseError).then(function () {
                        return sendRevisedNode(table, uuid).then(function () {
                            // For hooks to be able to work, we need to declare the
                            // tables that may be altered due to a change. In this case
                            // We want to allow adding pending upload photos.
                            // See for example dbSync.nodeChanges.hook().
                            return db.transaction('rw', table, dbSync.nodeChanges, dbSync.shardChanges,  dbSync.authorityPhotoUploadChanges, function() {
                                return table.get(uuid).catch(databaseError).then(function (node) {
                                    if (node) {
                                        var body = JSON.stringify({
                                            data: [node]
                                        });

                                        var response = new Response(body, {
                                            status: 200,
                                            statusText: 'OK',
                                            headers: {
                                                'Content-Type': 'application/json'
                                            }
                                        });

                                        var change = {
                                            type: type,
                                            uuid: uuid,
                                            method: 'PATCH',
                                            data: json,
                                            timestamp: Date.now()
                                        };

                                        var changeTable = dbSync.nodeChanges;
                                        var addShard = Promise.resolve();

                                        if (table === dbSync.shards) {
                                            changeTable = dbSync.shardChanges;

                                            addShard = table.get(uuid).catch(databaseError).then(function (item) {
                                                if (item) {
                                                    change.shard = item.shard;
                                                }
                                                else {
                                                    return Promise.reject('Unexpectedly could not find: ' + uuid);
                                                }
                                            });
                                        }

                                        return addShard.then(function () {
                                            change.isSynced = 0;
                                            return changeTable.add(change).then(function (localId) {
                                                return Promise.resolve(response);
                                            });
                                        });
                                    }
                                    else {
                                        return Promise.reject("UUID unexpectedly not found.");
                                    }
                              });
                            });
                        });
                    });
                });
            });
        }).catch(sendErrorResponses);
    }

    function postNode (request, type) {
        return dbSync.open().catch(databaseError).then(function () {
            return getTableForType(type).then(function (table) {
                return request.json().catch(jsonError).then(function (json) {
                    return makeUuid().then(function (uuid) {
                        json.uuid = uuid;
                        json.type = type;
                        json.status = Status.published;

                        // Not entirely clear whose job it should be to figure
                        // out the shard, but we'll do it here for now.
                        var addShard = Promise.resolve(json);

                        if (table === dbSync.shards) {
                            addShard = determineShard(json).then(function (shard) {
                                json.shard = shard;

                                return Promise.resolve(json);
                            });
                        }

                        return addShard.then(function (json) {
                            return table.put(json).catch(databaseError).then(function () {
                                return sendRevisedNode(table, uuid).then(function () {
                                    // For hooks to be able to work, we need to declare the
                                    // tables that may be altered due to a change. In this case
                                    // We want to allow adding pending upload photos.
                                    // See for example dbSync.nodeChanges.hook().
                                    return db.transaction('rw', table, dbSync.nodeChanges, dbSync.shardChanges, dbSync.authorityPhotoUploadChanges, function() {
                                          var body = JSON.stringify({
                                              data: [json]
                                          });

                                          var response = new Response(body, {
                                              status: 200,
                                              statusText: 'OK',
                                              headers: {
                                                  'Content-Type': 'application/json'
                                              }
                                          });

                                          var change = {
                                              type: type,
                                              uuid: uuid,
                                              method: 'POST',
                                              data: json,
                                              timestamp: Date.now(),
                                              // Mark entity as not synced.
                                              isSynced: 0
                                          };

                                          var changeTable = dbSync.nodeChanges;

                                          if (table === dbSync.shards) {
                                              changeTable = dbSync.shardChanges;
                                              change.shard = json.shard;
                                          }

                                          return changeTable.add(change).then(function (localId) {
                                              return Promise.resolve(response);
                                          });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        }).catch(sendErrorResponses);
    }

    function view (type, uuid) {
        return dbSync.open().catch(databaseError).then(function () {

            return getTableForType(type).then(function (table) {
              var uuids = uuid.split(',');

              // The key to query by.
              var key = 'uuid';
              var query =  table.where(key).anyOf(uuids).and(function (item) {
                return item.deleted === false;
              });

              return query.toArray().catch(databaseError).then(function (nodes) {
                  // We could also check that the type is the expected type.
                  if (nodes) {

                      var body = JSON.stringify({
                          data: nodes
                      });

                      var response = new Response(body, {
                          status: 200,
                          statusText: 'OK',
                          headers: {
                              'Content-Type': 'application/json'
                          }
                      });

                      return Promise.resolve(response);
                  } else {
                      response = new Response('', {
                          status: 404,
                          statusText: 'Not found'
                      });

                      return Promise.reject(response);
                  }
              });
          });
        }).catch(sendErrorResponses);
    }

    // A list of all types of measurements that can be
    // taken during groups encounter.
    var groupMeasurementTypes = [
      'attendance',
      'counseling_session',
      'child_fbf',
      'family_planning',
      'height',
      'lactation',
      'mother_fbf',
      'muac',
      'nutrition',
      'participant_consent',
      'photo',
      'weight'
    ];

    // This is a kind of special-case for now, at least. We're wanting to get
    // back all of measurements for whom the key is equal to the value.
    //
    // Ultimately, it would be better to make this more generic here and do the
    // processing on the client side, but this mirrors the pre-existing
    // division of labour between client and server, so it's easier for now.
    function viewMeasurements (key, uuid) {
        // UUID may be multiple list of UUIDs, so we split by it.
        var uuids = uuid.split(',');

        var query = dbSync.shards.where(key).anyOf(uuids);

        // Build an empty list of measurements, so we return some value, even
        // if no measurements were ever taken.
        var data = {};
        uuids.forEach(function(uuid) {
            data[uuid] = {};
            // Decoder is expecting to have the Person's UUID.
            data[uuid].uuid = uuid;
        });

        return query.toArray().catch(databaseError).then(function (nodes) {
            if (nodes) {
                nodes.forEach(function (node) {
                    var target = node.person;
                    if (key === 'person') {
                        // Check that node type for group encounter is one
                        // of mother / child measurements. See full list at
                        // groupMeasurementTypes array.
                        if (!groupMeasurementTypes.includes(node.type)) {
                          return;
                        }
                    }
                    else if (key === 'prenatal_encounter') {
                        target = node.prenatal_encounter;
                    }
                    else if (key === 'nutrition_encounter') {
                        target = node.nutrition_encounter;
                    }
                    else if (key === 'acute_illness_encounter') {
                      target = node.acute_illness_encounter;
                    }

                    data[target] = data[target] || {};
                    if (data[target][node.type]) {
                        data[target][node.type].push(node);
                    } else {
                        data[target] = data[target] || {};
                        data[target][node.type] = [node];
                    }
                });

                var body = JSON.stringify({
                    // Decoder is expecting a list, so we use Object.values().
                    data: Object.values(data)
                });

                var response = new Response(body, {
                    status: 200,
                    statusText: 'OK',
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });

                return Promise.resolve(response);
            } else {
                response = new Response('', {
                    status: 404,
                    statusText: 'Not found'
                });

                return Promise.reject(response);
            }
        }).catch(sendErrorResponses);
    }

    // Fields which we index along with type, so we can search for them.
    var searchFields = [
        'adult',
        'pin_code',
        'clinic',
        'person',
        'related_to'
    ];

    function index (url, type) {
        var params = url.searchParams;

        var offset = parseInt(params.get('offset') || '0');
        var range = parseInt(params.get('range') || '0');
        var sortBy = '';

        return dbSync.open().catch(databaseError).then(function (db) {

            var criteria = {type: type};

            // We can do a limited kind of criteria ... can be expanded when
            // necessary. This is most efficient if we have a compound index
            // including all the criteria.
            searchFields.forEach(function (field) {
                var searchValue = params.get(field);

                if (searchValue) {
                    criteria[field] = searchValue;
                }
            });

            return getTableForType(type).then(function (table) {
                // For syncmetadata, we don't actually use the criteria
                if (type === 'syncmetadata') {
                    var query = dbSync.syncMetadata;
                    var countQuery = query;
                } else {
                    query = table.where(criteria);
                    countQuery = query.clone();

                    // Exclude deleted results.
                    query = query.and(function (item) {
                      return item.deleted == false;
                    });
                    countQuery = query.and(function (item) {
                      return item.deleted == false;
                    });
                }

                var modifyQuery = Promise.resolve();

                if (type === 'person') {
                    var nameContains = params.get('name_contains');

                    if (nameContains) {
                        // For the case when there's more than one word as an input,
                        // we generate an array of lowercase words.
                        var words = nameContains.split(' ');
                        words.forEach(function (word, index) {
                          words[index] = word.toLowerCase();
                        });

                        modifyQuery = modifyQuery.then(function () {
                            // We search for resulting persons that start with any of the input words (apply 'OR' condition).
                            query = table.where('name_search').startsWithAnyOf(words).distinct().and(function (person) {
                              // If person is marked as deleted, do not include it in search results.
                              if (person.deleted === true) {
                                return false;
                              }

                              // Now, we check that each word we got as search input is a prefix
                              // of any of person name parts (applying 'AND condition').
                              return words.every(function (word) {
                                return person.name_search.some(function (nameSearchWord) {
                                  return nameSearchWord.startsWith(word);
                                });
                              });
                            });

                            // Cloning doesn't seem to work for this one.
                            countQuery = table.where('name_search').startsWithAnyOf(words).distinct().and(function (person) {
                              if (person.deleted === true) {
                                return false;
                              }

                              return words.every(function (word) {
                                return person.name_search.some(function (nameSearchWord) {
                                  return nameSearchWord.startsWith(word);
                                });
                              });
                            });

                            sortBy = 'label';

                            return Promise.resolve();
                        });
                    }
                }

                // For PmtctParticipant, check the session param and (if
                // provided) get only those expected at the specified
                // session.
                if (type === 'pmtct_participant') {
                    var sessionId = params.get('session');
                    if (sessionId) {
                        modifyQuery = modifyQuery.then(function () {
                            return table.get(sessionId).then(function (session) {
                                if (session) {
                                    criteria.clinic = session.clinic;

                                    query = table.where(criteria).and(function (participation) {
                                        // If participation is marked as deleted, do not include it in results.
                                        if (participation.deleted === true) {
                                          return false;
                                        }

                                        return expectedOnDate(participation, session.scheduled_date.value);
                                    });

                                    countQuery = query.clone();

                                    return Promise.resolve();
                                } else {
                                    return Promise.reject('Could not find session: ' + sessionId);
                                }
                            });
                        });
                    }
                }

                if (type === 'prenatal_encounter' || type === 'nutrition_encounter' || type === 'acute_illness_encounter') {
                  var individualSessionId = params.get('individual_participant');
                  if (individualSessionId) {
                    modifyQuery = modifyQuery.then(function () {
                        criteria.individual_participant = individualSessionId;
                        query = table.where(criteria).and(function (encounter) {
                            // If encounter is marked as deleted, do not include it in results.
                            return encounter.deleted === false;
                        });

                        countQuery = query.clone();

                        return Promise.resolve();
                    });
                  }
                }

                // For session endpoint, check child param and only return
                // those sessions which the child was expected at.
                if (type === 'session') {
                    var childId = params.get('child');
                    if (childId) {
                        modifyQuery = modifyQuery.then(function () {
                            return table.where({
                                type: 'pmtct_participant',
                                person: childId
                            }).toArray().then(function (participations) {
                                var clinics = [];
                                participations.forEach(function(participation) {
                                    // If participation is marked as deleted, do not include it in results.
                                    if (participation.deleted === false) {
                                        clinics.push(['session', participation.clinic]);
                                    }
                                })

                                query = table.where('[type+clinic]').anyOf(clinics);

                                // Cloning doesn't seem to work for this one.
                                countQuery = table.where('[type+clinic]').anyOf(clinics);

                                return Promise.resolve();
                            });
                        });
                    }
                }

                return modifyQuery.then(function () {
                    return countQuery.count().catch(databaseError).then(function (count) {

                        if (offset > 0) {
                            query.offset(offset);
                        }

                        if (range > 0) {
                            query.limit(range);
                        }

                        var getNodes = sortBy ? query.sortBy(sortBy) : query.toArray();

                        return getNodes.catch(databaseError).then(function (nodes) {
                            var body = JSON.stringify({
                                offset: offset,
                                count: count,
                                data: nodes
                            });

                            var response = new Response(body, {
                                status: 200,
                                statusText: 'OK',
                                headers: {
                                    'Content-Type': 'application/json'
                                }
                            });

                            return Promise.resolve(response);
                        });
                    });
                });
            });
        }).catch(sendErrorResponses);
    }

    function determineShard (node) {
        // Shraded nodes that specifically specify their shard.
        // To be more precise, these are individual participants,
        // individual encounters, persons and relationships.
        // This check must be first, so the shard field would not get
        // overriden by health_center field, when both of them exist
        // at node - for example, at perosn node.
        if (node.shard) {
            return Promise.resolve(node.shard);
        }

        // Resolving for group measurements.
        if (node.session) {
            return dbSync.shards.get(node.session).then (function (session) {
                return resolveShardByClinicId(session.clinic)
            });
        }

        // Resolving for individual measurements.
        if (node.health_center) {
            return Promise.resolve(node.health_center);
        }

        // Resolving for pmtct_participant.
        if (node.clinic) {
            return resolveShardByClinicId(node.clinic);
        }

        return Promise.reject('Node ' + node.uuid + ' got no fields using which shard can be resolved!' );
    }

    function resolveShardByClinicId (clinicId) {
      return dbSync.shards.get(clinicId).then(function (clinic) {
          if (clinic) {
              if (clinic.health_center) {
                  return Promise.resolve(clinic.health_center);
              } else {
                  return Promise.reject('Clinic had no health_center: ' + clinic.uuid);
              }
          } else {
              return Promise.reject('Could not find clinic: ' + session.clinic);
          }
      });
    }

    function sendRevisedNode (table, uuid) {
        return table.get(uuid).catch(databaseError).then(function (item) {
            if (item) {
                return sendRevisions([item]);
            } else {
                return Promise.reject("UUID unexpectedly not found.");
            }
        });
    }

    // This is meant for the end of a promise chain. If we've rejected with a
    // `Response` object, then we resolve instead, so that we'll send the
    // response. (Otherwise, we'll send a network error).
    function sendErrorResponses (err) {
        if (err instanceof Response) {
            return Promise.resolve(err);
        } else {
            return Promise.reject(err);
        }
    }

    function databaseError (err) {
        var response = new Response(JSON.stringify(err), {
            status: 500,
            statusText: 'Database Error'
        });

        return Promise.reject(response);
    }

    function jsonError (err) {
        var response = new Response(JSON.stringify(err), {
            status: 400,
            statusText: 'Bad JSON'
        });

        return Promise.reject(response);
    }

    // For things created on the backend, we use a v5 UUID which is a
    // combination of a v4 device UUID and a high-res timestamp. So, we'll do
    // the same thing here.  That is, we'll generate a v4 device UUID, and
    // we'll use it with a high-res timestamp to create a v5 UUID. That ought
    // to provide a sufficient guarantee of no UUID collisions.
    function makeUuid () {
        var timestamp = String(performance.timeOrigin + performance.now());

        return caches.open(configCache).then(function (cache) {
            return cache.match(deviceUuidUrl).then(function (response) {
                if (response) {
                    return response.text();
                } else {
                    var uuid = kelektivUuid.v4();

                    var cachedResponse = new Response(uuid, {
                        status: 200,
                        statusTest: 'OK',
                        headers: {
                            'Content-Type': 'application/text'
                        }
                    });

                    var cachedRequest = new Request (deviceUuidUrl, {
                        method: 'GET'
                    });

                    return cache.put(cachedRequest, cachedResponse).then(function () {
                        return Promise.resolve(uuid);
                    });
                }
            });
        }).then(function (deviceUuid) {
            return Promise.resolve(kelektivUuid.v5(timestamp, deviceUuid));
        });
    }

    /**
     * Helper function to add a record to authority PhotoUploadChanges.
     */
    function addPhotoUploadChanges(tableHook, table, obj) {
        if (!obj.data.hasOwnProperty('photo')) {
            // Entity doesn't have a photo.
            return;
        }

        if (!photosUploadUrlRegex.test(obj.data.photo)) {
            // Photo URL doesn't point to the local cache.
            return;
        }

        // As `localId`, the primary key, is auto incremented, we have to wait
        // for the transaction to finish in order to have it.
        tableHook.onsuccess = async function (primaryKey) {
            const result = await table.add({
                localId : primaryKey,
                uuid: obj.uuid,
                photo: obj.data.photo,
                // Drupal's file ID.
                fileId: null,
                // The file name on Drupal.
                remoteFileName: null,
                // Indicate photo was not uploaded to Drupal yet.
                // IndexDB doesn't index Boolean, so we use an Int.
                isSynced: 0,
            });

            return result;
        };
    }

})();
