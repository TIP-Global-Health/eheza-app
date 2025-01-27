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

(() => {

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

    self.addEventListener('fetch', event => {
        var url = new URL(event.request.url);
        var matches = nodesUrlRegex.exec(url.pathname);

        if (matches) {
            var type = matches[1];
            var uuid = matches[2]; // May be null

            event.respondWith(handleEvent(event, url, type, uuid));
        }
    });

    async function handleEvent(event, url, type, uuid) {
        var notFoundResponse = new Response('', {
            status: 404,
            statusText: 'Not Found'
        });

        // If placeholder still indicates tha DB was not initialized,
        // or version of initialized DB is not as we expect,
        // initialize it.
        if (dbSync === null || dbSync.verno != dbVerno) {
            // Check if IndexedDB exists.
            var dbExists = await Dexie.exists('sync');
            if (!dbExists) {
              // Skip any further actions, if it's not.
              return notFoundResponse;
            }

            // Redefine tables properties.
            dbSync = new Dexie('sync');
            db = await dbSync.open();
            db.tables.forEach(function(table) {
                dbSync[table.name] = table;
            });

            // This hook is activated as a result of new content being created on device.
            dbSync.shards.hook('creating', function (primKey, obj, trans) {
              if (obj.type === 'person') {
                if (typeof obj.label == 'string') {
                  obj.name_search = gatherWords(obj.label);
                }
              }
            });

            // This hook is activated as a result of content being edited on device.
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

        if (event.request.method === 'GET') {
            if (uuid) {
                if (type === 'child-measurements' || type === 'mother-measurements') {
                    return viewMeasurements('person', uuid);
                }
                else if (type === 'prenatal-measurements') {
                    return viewMeasurements('prenatal_encounter', uuid);
                }
                else if (type === 'nutrition-measurements') {
                    return viewMeasurements('nutrition_encounter', uuid);
                }
                else if (type === 'acute-illness-measurements') {
                    return viewMeasurements('acute_illness_encounter', uuid);
                }
                else if (type === 'home-visit-measurements') {
                    return viewMeasurements('home_visit_encounter', uuid);
                }
                else if (type === 'well-child-measurements') {
                    return viewMeasurements('well_child_encounter', uuid);
                }
                else if (type === 'ncd-measurements') {
                    return viewMeasurements('ncd_encounter', uuid);
                }
                else if (type === 'child-scoreboard-measurements') {
                  return viewMeasurements('child_scoreboard_encounter', uuid);
                }
                else if (type === 'tuberculosis-measurements') {
                  return viewMeasurements('tuberculosis_encounter', uuid);
                }
                else if (type === 'hiv-measurements') {
                    return viewMeasurements('hiv_encounter', uuid);
                }
                else if (type === 'follow-up-measurements') {
                    return viewFollowUpMeasurements(uuid);
                }
                else if (type === 'stock-management-measurements') {
                    return viewStockManagementMeasurements(uuid);
                }
                else if (type === 'pregnancy-by-newborn') {
                    return viewMeasurements('newborn', uuid);
                }
                else {
                    return view(type, uuid);
                }
            } else {
                  return index(url, type);
            }
        }

        if (event.request.method === 'DELETE') {
            if (uuid) {
                return deleteNode(url, type, uuid);
            }
        }

        if (event.request.method === 'PUT') {
            if (uuid) {
                return patchNode(event.request, type, uuid);
            }

            return postNode(event.request, type);
        }

        if (event.request.method === 'POST') {
            return postNode(event.request, type);
        }

        if (event.request.method === 'PATCH') {
            if (uuid) {
                return patchNode(event.request, type, uuid);
            }
        }

        // If we get here, respond with a 404
        return notFoundResponse;
    }

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
                return dbSync.syncMetadata.delete(uuid).catch(databaseError).then(function () {
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
                            var tableName = table.name;
                            return db.transaction('rw', tableName, dbSync.nodeChanges, dbSync.shardChanges,  dbSync.authorityPhotoUploadChanges, function() {
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
                                            timestamp: Date.now(),
                                            // Mark entity as not synced.
                                            isSynced: 0
                                        };

                                        var changeTable = dbSync.nodeChanges;
                                        var addShard = Promise.resolve();

                                        if (tableName === 'shards') {
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
                                            return changeTable.add(change).catch(function (err) {
                                                // If there was a failure, we try again, assuming it
                                                // may have been a glitch. If operation fails again,
                                                // we log error with Rollbar.
                                                return changeTable.add(change).catch(function (err) {
                                                    var reject = new Response(body, {
                                                        status: 400,
                                                        statusText: 'Failure: PATCH at changes table'
                                                    });

                                                    return Promise.resolve(reject);
                                                }).then(function (localId) {
                                                    return Promise.resolve(response);
                                                });
                                            }).then(function (localId) {
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
                                    var tableName = table.name;
                                    // For hooks to be able to work, we need to declare the
                                    // tables that may be altered due to a change. In this case
                                    // We want to allow adding pending upload photos.
                                    // See for example dbSync.nodeChanges.hook().
                                    return db.transaction('rw', tableName, dbSync.nodeChanges, dbSync.shardChanges, dbSync.authorityPhotoUploadChanges, function() {
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

                                          if (tableName === 'shards') {
                                              changeTable = dbSync.shardChanges;
                                              change.shard = json.shard;
                                          }

                                          return changeTable.add(change).catch(function (err) {
                                              // If there was a failure, we try again, assuming it
                                              // may have been a glitch. If operation fails again,
                                              // we log error with Rollbar.
                                              return changeTable.add(change).catch(function (err) {
                                                  var reject = new Response(body, {
                                                      status: 400,
                                                      statusText: 'Failure: POST at changes table'
                                                  });

                                                  return Promise.resolve(reject);
                                              }).then(function (localId) {
                                                  return Promise.resolve(response);
                                              });
                                          }).then(function (localId) {
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
      'contributing_factors',
      'counseling_session',
      'child_fbf',
      'family_planning',
      'follow_up',
      'group_health_education',
      'group_ncda',
      'group_send_to_hc',
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
                    else if (key === 'home_visit_encounter') {
                        target = node.home_visit_encounter;
                    }
                    else if (key === 'well_child_encounter') {
                        target = node.well_child_encounter;
                    }
                    else if (key === 'ncd_encounter') {
                        target = node.ncd_encounter;
                    }
                    else if (key === 'child_scoreboard_encounter') {
                        target = node.child_scoreboard_encounter;
                    }
                    else if (key === 'tuberculosis_encounter') {
                        target = node.tuberculosis_encounter;
                    }
                    else if (key === 'hiv_encounter') {
                        target = node.hiv_encounter;
                    }
                    else if (key === 'newborn') {
                        target = node.newborn;
                    }

                    data[target] = data[target] || {};
                    if (data[target][node.type]) {
                        data[target][node.type].push(node);
                        if (key !== 'person') {
                          // Sorting DESC, so that node with highest vid
                          // is selected first, as it was edited last, and
                          // got most recent data.
                          data[target][node.type].sort((a,b) => (b.vid - a.vid));
                        }
                    } else {
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

    // List with all types of Follow Up measurements.
    // Follow Ups get resolved using date_concluded field.
    var followUpMeasurementsTypes = [
      'acute_illness_follow_up',
      'follow_up',
      'nutrition_follow_up',
      'prenatal_follow_up',
      'well_child_follow_up',
      'tuberculosis_follow_up',
      'hiv_follow_up',
      'acute_illness_trace_contact',
      'prenatal_labs_results',
      'ncd_labs_results',
      'well_child_next_visit'
    ];

    // These are types of follow-ups that need to be loaded, even if they
    // were resolved during period of past 6 months.
    // This is required to present data at Dashboard statistics.
    var followUpMeasurementsTypesUsedByDashboard = [
      'acute_illness_follow_up',
      'follow_up',
      'nutrition_follow_up',
      'prenatal_follow_up',
      'well_child_follow_up',
    ];

    // These are HIV tests, where HIV positive patient can be diagnosed.
    // We need them since HIV followup at case management should appear
    // when patient was diagniosed with HIV when taking a test, and did not
    // have HIV encounter after.
    var hivTestTypes = [
      'ncd_hiv_test',
      'prenatal_hiv_test',
    ]

    function viewFollowUpMeasurements (shard) {
        // Load all types of follow up measurements, and HIV test results
        // that belong to provided healh center.
        var typesToLoad = followUpMeasurementsTypes.concat(hivTestTypes);
        var query = dbSync.shards.where('type').anyOf(typesToLoad).and(function (item) {
          return item.shard === shard;
        });

        // Build an empty list of measurements, so we return some value, even
        // if no measurements were ever taken.
        var data = {};
        // Decoder is expecting to have the health center UUID.
        data.uuid = shard;

        return query.toArray().catch(databaseError).then(function (nodes) {
            if (nodes) {
                var today = new Date();
                var patientsWithHIVFollowUps = [];
                nodes.forEach(function (node) {
                    // Do not process nodes that are not follow ups.
                    if (hivTestTypes.includes(node.type)) {
                      return;
                    }

                    // Record IDs of patients that have any HIV follow up.
                    if (node.type == 'hiv_follow_up') {
                      if (patientsWithHIVFollowUps.indexOf(node.person) === -1) {
                        patientsWithHIVFollowUps.push(node.person);
                      }
                    }

                    if (node.date_concluded != undefined && typeof node.date_concluded != 'undefined') {
                        var resolutionDate = new Date(node.date_concluded);
                        if (followUpMeasurementsTypesUsedByDashboard.includes(node.type)) {
                          resolutionDate.setMonth(today.getMonth() + 6);
                        }

                        if (resolutionDate < today) {
                          return;
                        }
                    }

                    if (data[node.type]) {
                        data[node.type].push(node);
                    } else {
                        data[node.type] = [node];
                    }
                });

                // Recording all patients that had posiitve HIV test result.
                // In case of multiple posiitve results for a patient, we
                // record most recent test date.
                var positiveHIVMap = {};
                nodes.forEach(function (node) {
                  // Do not process follow ups nodes.
                  if (!hivTestTypes.includes(node.type)) {
                    return;
                  }

                  // Do not process, if test result is not positive.
                  if (node.test_result !== 'positive') {
                    return;
                  }

                  // First time positive result for patient is found - recorded.
                  if (!positiveHIVMap[node.person]) {
                    positiveHIVMap[node.person] = {id: node.person, date: node.date_measured, uuid: node.uuid};
                    return;
                  }

                  // Another positive result for patient is found - record the
                  // most recent one.
                  var current = new Date(positiveHIVMap[node.person].date);
                  var candidate = new Date(node.date_measured);
                  if (current < candidate) {
                    positiveHIVMap[node.person] = {id: node.person, date: node.date_measured, uuid: node.uuid};
                  }
                });

                // Creating 'dummy' HIV follow ups for patients that have positive HIV
                // result, and never had HIV follow up (which means that they were)
                // never diagnosed HIV posiitve during HIV encounter.
                Object.values(positiveHIVMap).forEach((item) => {
                  if (patientsWithHIVFollowUps.indexOf(item.id) !== -1) {
                    // Patinet has HIV follow up - skip to next one.
                    return;
                  }

                  // Create 'dummy' HIV follow up.
                  var hivFollowUp = {
                    date_concluded: null,
                    // Positive HIV test result date.
                    date_measured: item.date,
                    deleted: false,
                    // Per requirements, positive HIV test result follow up is
                    // to be scheduled to 1 week.
                    follow_up_options: ['1-w'],
                    health_center: null,
                    // This will be uesd as an indicator for front-end, to understand
                    // that this follow up represents positive HIV test.
                    hiv_encounter: 'dummy',
                    nurse: 'dummy',
                    person: item.id,
                    shard: 'dummy',
                    type: 'hiv_follow_up',
                    // We only need to have the UUID unique, so we use
                    // the UUID pf positive HIV test node.
                    // We don't perform any editing on fornt-end, so it's
                    // sufficient.
                    uuid: item.uuid
                  };

                  // Add 'dummy' HIV follow up to the data.
                  if (data['hiv_follow_up']) {
                      data['hiv_follow_up'].push(hivFollowUp);
                  } else {
                      data['hiv_follow_up'] = [hivFollowUp];
                  }
                });

                var body = JSON.stringify({
                    // Decoder is expecting a list.
                    data: [data]
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

    var stockManagementMeasurementsTypes = [
      'child_fbf',
      'mother_fbf',
      'stock_update'
    ];

    function viewStockManagementMeasurements (shard) {
        // Load all types of follow up measurements that belong to provided healh center.
        var query = dbSync.shards.where('type').anyOf(stockManagementMeasurementsTypes).and(function (item) {
          return item.shard === shard;
        });

        // Build an empty list of measurements, so we return some value, even
        // if no measurements were ever taken.
        var data = {};
        data = {};
        // Decoder is expecting to have the health center UUID.
        data.uuid = shard;

        return query.toArray().catch(databaseError).then(function (nodes) {
            if (nodes) {
                nodes.forEach(function (node) {
                    if (data[node.type]) {
                        data[node.type].push(node);
                    } else {
                        data[node.type] = [node];
                    }
                });

                var body = JSON.stringify({
                    // Decoder is expecting a list.
                    data: [data]
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
                    // There're 3 options for SINGLE param that's passed.
                    // No need ot wory about combinations of several params.
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

                    // Second option for param.
                    var nationalIdContains = params.get('national_id_contains');
                    if (nationalIdContains) {
                        modifyQuery = modifyQuery.then(function () {
                            // We search for resulting persons that start with any of the input words (apply 'OR' condition).
                            query = table.where('national_id_number').startsWith(nationalIdContains).distinct().and(function (person) {
                              // If person is marked as deleted, do not include it in search results.
                              return (person.deleted !== true);
                            });

                            // Cloning doesn't seem to work for this one.
                            countQuery = table.where('national_id_number').startsWith(nationalIdContains).distinct().and(function (person) {
                              // If person is marked as deleted, do not include it in search results.
                              return (person.deleted !== true);
                            });

                            sortBy = 'label';

                            return Promise.resolve();
                        });
                    }

                    // Third option for param.
                    var geoFields = params.get('geo_fields');
                    if (geoFields) {
                        var fields = geoFields.split('|');
                        modifyQuery = modifyQuery.then(function () {
                            criteria.province = fields[0];
                            criteria.district = fields[1];
                            criteria.sector = fields[2];
                            criteria.cell = fields[3];
                            criteria.village = fields[4];
                            query = table.where(criteria);

                            countQuery = query.clone();

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

                if (type === 'individual_participant') {
                  var people = params.get('people');
                  if (people) {
                    var uuids = people.split(',');
                    var tuples = uuids.map((uuid) => [type, uuid]);
                    modifyQuery = modifyQuery.then(function () {
                        query = table.where('[type+person]').anyOf(tuples).and(function (participant) {
                            // If participant is marked as deleted, do not include it in results.
                            return participant.deleted === false;
                        });

                        // Cloning doesn't seem to work for this one.
                        // If done, it corrupts the results of original query.
                        countQuery = table.where('[type+person]').anyOf(tuples).and(function (participant) {
                            return participant.deleted === false;
                        });;

                        return Promise.resolve();
                    });
                  }
                }

                var encounterTypes = [
                  'acute_illness_encounter',
                  'child_scoreboard_encounter',
                  'hiv_encounter',
                  'home_visit_encounter',
                  'ncd_encounter',
                  'nutrition_encounter',
                  'prenatal_encounter',
                  'tuberculosis_encounter',
                  'well_child_encounter'
                ];
                if (encounterTypes.includes(type)) {
                  var participantIds = params.get('individual_participants');
                  if (participantIds) {
                    var uuids = participantIds.split(',');
                    var tuples = uuids.map((uuid) => [type, uuid]);
                    modifyQuery = modifyQuery.then(function () {
                        // Encounters curently don't have option to be deleted,
                        // so there's no need to check for that.
                        query = table.where('[type+individual_participant]').anyOf(tuples);

                        // Cloning doesn't seem to work for this one.
                        // If done, it corrupts the results of original query.
                        countQuery = table.where('[type+individual_participant]').anyOf(tuples);

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
                                // If done, it corrupts the results of original query.
                                countQuery = table.where('[type+clinic]').anyOf(clinics);

                                return Promise.resolve();
                            });
                        });
                    }
                }

                // Resilience surveys are pulled for a nurse,
                // so we add criteria to filter by provided nurse ID.
                if (type === 'resilience_survey') {
                  var nurseId = params.get('nurse');
                  if (nurseId) {
                    modifyQuery = modifyQuery.then(function () {
                        criteria.nurse = nurseId;
                        query = table.where(criteria);

                        countQuery = query.clone();

                        return Promise.resolve();
                    });
                  }
                }

                // For education_session endpoint, check participant param and
                // only return those sessions were participant has participated.
                if (type === 'education_session') {
                  var personId = params.get('participant');
                  if (personId) {
                    modifyQuery = modifyQuery.then(function () {
                        criteria.participating_patients = personId;
                        query = table.where(criteria);

                        countQuery = query.clone();

                        return Promise.resolve();
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
        var url;

        if (obj.data.hasOwnProperty('photo')) {
          url = obj.data.photo;
        }
        else if (obj.data.hasOwnProperty('signature')) {
          url = obj.data.signature;
        }
        else {
          // Entity doesn't have an image.
          return;
        }

        if (!photosUploadUrlRegex.test(url)) {
            // Photo URL doesn't point to the local cache.
            return;
        }

        // As `localId`, the primary key, is auto incremented, we have to wait
        // for the transaction to finish in order to have it.
        tableHook.onsuccess = async function (primaryKey) {
            const result = await table.add({
                localId : primaryKey,
                uuid: obj.uuid,
                photo: url,
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
