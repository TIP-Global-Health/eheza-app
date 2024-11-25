// Normally, you'd want to do this on the server, but there doesn't seem to be
// a mechanism for it on Pantheon, since the request for the app doesn't hit
// the PHP code.
if ((location.hostname.endsWith('pantheonsite.io') || (location.hostname ===
    'example-pantheonsite.io')) && location.protocol == 'http:') {
  // This will do a redirect
  location.protocol = 'https:';
}

var dbSync = new Dexie('sync');

dbSync.version(1).stores({
  // It's not entirely clear whether it will be more convenient to split
  // up the content-types into their own stores, or keep them together.
  // Intuitively, it seems as though it will be more convenient to keep
  // them together, but we can revisit that if necessary. IndexedDB is
  // fundamentally a NoSQL-type database, so each item need not have
  // the same shape. And, there are no SQL-type joins, so using many
  // stores is inconvenient.
  //
  // (It turns out that you can only use one IndexedDB index at a time.
  // This will make for a lot of indexes -- it may be nicer to split up
  // the types into different tables. However, that will make it harder
  // to calculate the maximum `vid`. So, we'll see).
  //
  // What we're specifying here is a comma-separate list of the fields to
  // index. The first field is the primary key, and the `&` indicates
  // that it should be unique.
  nodes: '&uuid,type,vid,status,[type+pin_code],[type+clinic],[type+mother]',

  // We'll write local changes here and eventually upload them.
  nodeChanges: '++localId',

  // Metadata that tracks information about the sync process. The uuid is the
  // UUID of the shard we are syncing. So, for things we sync by health
  // center, it's the UUID of the health center. For things in the nodes
  // table, which every device gets, we use a static UUID here (`nodesUuid`).
  // @DEPRECATED
  syncMetadata: '&uuid',

  // This is like the `nodes` table, but for the things that we don't
  // download onto all devices -- that is, for the things for which we are
  // "sharding" the database by health center.
  //
  // The `uuid` is the UUID of the node. The `shard` is the UUID of the
  // health center which is the reason we're downloading this node to this
  // device. We need a compound key with shard and vid, because IndexedDb
  // is a bit weird about using indexes -- you can only use one at a time.
  shards: '&uuid,type,vid,status,child,mother,[shard+vid]',

  // Write local changes here and eventually upload.
  shardChanges: '++localId,shard'
});

dbSync.version(2).stores({
  nodes: '&uuid,type,vid,status,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to]',
  shards: '&uuid,type,vid,status,person,[shard+vid]',
}).upgrade(function (tx) {
  // On upgrading to version 2, clear nodes and shards.
  return tx.nodes.clear().then(function () {
    return tx.shards.clear();
  }).then(function () {
    // And reset sync metadata.
    return tx.syncMetadata.toCollection().modify(function (data) {
      delete data.download;
      delete data.upload;

      data.attempt = {
        tag: 'NotAsked',
        timestamp: Date.now()
      };
    });
  });
});

dbSync.version(3).stores({
  nodes: '&uuid,type,vid,status,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
});

dbSync.version(4).stores({
  nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
}).upgrade(function (tx) {
  return tx.nodes.where({
    type: 'person'
  }).modify(function (person) {
    person.name_search = gatherWords(person.label);
  });
});

dbSync.version(5).stores({
  nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
}).upgrade(function (tx) {
  return tx.nodes.where({
    type: 'clinic'
  }).delete();
});

dbSync.version(6).stores({
  nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+adult]',
}).upgrade(function (tx) {
  return tx.nodes.where({
    type: 'participant_form'
  }).delete();
});

dbSync.version(7).stores({
  nodes: '&uuid,type,vid,status,*name_search,[type+pin_code],[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
  shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter',
}).upgrade(function (tx) {
  return tx.nodes.where({
    type: 'session'
  }).delete();
});

dbSync.version(8).upgrade(function (tx) {
  return tx.nodes.where({
    type: 'clinic'
  }).delete().then(function () {
    return tx.nodes.where({
      type: 'nurse'
    }).delete();
  });
});

dbSync.version(9).stores({
  shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter',
});

dbSync.version(10).stores({
    nodes: '&uuid,type,vid,status,[type+pin_code]',
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(11).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(12).stores({
    statistics: '&uuid',
});

dbSync.version(13).upgrade(function (tx) {
    return tx.nodes.toCollection().modify(function (node) {
        node.deleted = false;
    })
    .then(function () {
        tx.shards.toCollection().modify(function (shard) {
          shard.deleted = false;
        });
    });
});

dbSync.version(14).stores({
  // Add `isSynced` and `uuid` indices so we would have an indication to when we
  // can delete local changes. Only after we download from the backend, we'd
  // want to delete the records.
  nodeChanges: '++localId,uuid,isSynced',
  shardChanges: '++localId,shard,uuid,isSynced',


  // Hold table with photos which have not been downloaded yet.
  // `attempts` holds the number of attempts we've tried to get the image.
  deferredPhotos: '&uuid,type,vid,photo,attempts',

  // Keep a list of the photos the need to be uploaded to the backend.
  // The `localId` corresponds to the `localId` present in the `nodeChanges`
  // The `file` is the file ID we will eventually get from Drupal, once
  // the file is uploaded. This is the the file ID we would need to insert into
  // the request, when creating or editing an entity such as a photo.
  // This property is a Maybe value, as at the time of creation of the photo
  // locally, we still don't have it.
  // `isSynced` is a boolean index, since IndexDB doesn't support IsNull query,
  // but we would need to fetch photos who were not synced yet.
  // Photos (such as a person photo, or nutrition photo), exist currently only
  // in Authority context, that is why we don't have an equivalent
  // `generalPhotoUploadChanges`.
  authorityPhotoUploadChanges: '&localId,photo,fileId,isSynced',
}).upgrade(function (tx) {
  // Get the data from the deprecated `syncMetadata` and move to local storage.
  (async () => {
    const collection = await tx.syncMetadata.toCollection().toArray();

    var syncInfoAuthorities = [];
    var itemsToProcess = collection.length;
    collection.forEach(async function(row) {
        // Check if this is the General sync UUID, and if so migrate syncMetadata into Local  storage.
        if (row.uuid == '78cf21d1-b3f4-496a-b312-d8ae73041f09') {
            var syncInfoGeneral = {lastFetchedRevisionId: 0, remainingToUpload:0, remainingToDownload: 0, status: 'Not Available'};
            syncInfoGeneral.deviceName = row.download.device_name;
            syncInfoGeneral.lastSuccesfulContact = row.download.last_contact;

            // Pulling latest revision that was synced.
            let result = await dbSync
              .nodes
              .where('vid')
              .above(0)
              .reverse()
              .limit(1)
              .sortBy('vid');

              // If we managed to get non empty result from the query above (limited to 1 result),
              // set latest synced revision to and set status to 'Success'.
            if (result.length == 1) {
                syncInfoGeneral.lastFetchedRevisionId = result[0].vid;
                syncInfoGeneral.status = 'Success';
            }

            localStorage.setItem('syncInfoGeneral', JSON.stringify(syncInfoGeneral));
            itemsToProcess--;
        }
        // This is sync data of a health center.
        else {
            var syncInfoAuthority = {lastFetchedRevisionId: 0, remainingToUpload:0, remainingToDownload: 0, status: 'Not Available'};
            syncInfoAuthority.lastSuccesfulContact = row.download.last_contact;
            syncInfoAuthority.statsCacheHash = '';
            syncInfoAuthority.uuid = row.uuid;

            let result = await dbSync
              .shards
              .where('[shard+vid]').between(
                  [row.uuid, Dexie.minKey],
                  [row.uuid, Dexie.maxKey]
              )
              .reverse()
              .limit(1)
              // Get the most recent record.
              .sortBy('vid');

              // If we managed to get non empty result from the query above (limited to 1 result),
              // set latest synced revision to and set status to 'Success'.
            if (result.length == 1) {
                syncInfoAuthority.lastFetchedRevisionId = result[0].vid;
                syncInfoAuthority.status = 'Success';
            }

            syncInfoAuthorities.push(syncInfoAuthority);
            itemsToProcess--;
        }

        // If we've processed all the items, store Authorities
        // sync info, and refresh, so that the APP reinitializes
        // with the sync data from  Local Storage.
        if (itemsToProcess === 0) {
            localStorage.setItem('syncInfoAuthorities', JSON.stringify(syncInfoAuthorities));
            location.reload();
        }
    });

    return Promise.resolve();
  })();
});

dbSync.version(15).upgrade(function (tx) {
    return tx.shardChanges.toCollection().modify(function (node) {
        node.isSynced = 0;
    })
});

dbSync.version(16).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(17).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,well_child_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(18).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,well_child_encounter,ncd_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult]',
});

dbSync.version(19).stores({
    whatsAppUploads: '++localId,screenshot,report_type,person,phone_number,fileId,syncStage',
});

dbSync.version(20).stores({
    nodes: '&uuid,type,vid,status,[type+pin_code],[type+nurse]'
});

dbSync.version(21).stores({
    errorsHash: '++localId, hash',
    dbErrors: '++localId, error, isSynced'
});

dbSync.version(22).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,well_child_encounter,ncd_encounter,child_scoreboard_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult],newborn',
});

dbSync.version(23).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,well_child_encounter,ncd_encounter,child_scoreboard_encounter,tuberculosis_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult],newborn',
});

dbSync.version(24).stores({
    whatsAppUploads: '++localId,screenshot,language,report_type,person,phone_number,fileId,syncStage',
});

dbSync.version(25).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,well_child_encounter,ncd_encounter,child_scoreboard_encounter,tuberculosis_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult],newborn,*participating_patients',
});

dbSync.version(26).stores({
    shards: '&uuid,type,vid,status,person,[shard+vid],prenatal_encounter,nutrition_encounter,acute_illness_encounter,home_visit_encounter,well_child_encounter,ncd_encounter,child_scoreboard_encounter,tuberculosis_encounter,hiv_encounter,*name_search,[type+clinic],[type+person],[type+related_to],[type+person+related_to],[type+individual_participant],[type+adult],[type+province+district+sector+cell+village],newborn,*participating_patients',
});

dbSync.version(27).upgrade(function (tx) {
  return tx.deferredPhotos.clear();
});

dbSync.version(28).upgrade(function (tx) {
  return tx.nodeChanges.clear();
});

dbSync.version(29).upgrade(function (tx) {
  return tx.nodeChanges.clear();
});
/**
 * --- !!! IMPORTANT !!! ---
 *
 * When creating new DB version, update:
 *
 * 1. dbVersion constant below (app.js)
 * 2. dbVerno constant at sw.js
 * 3. HEDLEY_RESTFUL_CLIENT_SIDE_INDEXEDDB_SCHEMA_VERSION at hedley_restful.module
 */

// This hook is activated as a result of new content that is being synced from backend.
dbSync.shards.hook('creating', function (primKey, obj, trans) {
  if (obj.type === 'person') {
    if (typeof obj.label == 'string') {
      obj.name_search = gatherWords(obj.label);
    }
  }
});

// This hook is activated as a result of updated content that is being synced from backend.
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

function gatherWords (text) {
  // Split on spaces, and remove blanks from result.
  return (text || '').split(/\s+/).flatMap(function (word) {
    if (word) {
      return [word.toLowerCase()];
    } else {
      return [];
    }
  });
}

/**
 * The DB version on the backend.
 *
 * This must be sent whenever we POST or PATCH an entity to the backend.
 *
 * @type {number}
 */
const dbVersion = 29;

/**
 * Return saved info for General sync.
 */
const getSyncInfoGeneral = function() {
  const storage = localStorage.getItem('syncInfoGeneral');

  if (!!storage) {
    let storageArr = JSON.parse(storage);
    storageArr.lastFetchedRevisionId = parseInt(storageArr.lastFetchedRevisionId);
    storageArr.lastSuccesfulContact = parseInt(storageArr.lastSuccesfulContact);
    storageArr.remainingToUpload = parseInt(storageArr.remainingToUpload);
    storageArr.remainingToDownload = parseInt(storageArr.remainingToDownload);

    if (storageArr.rollbarToken === undefined) {
      storageArr.rollbarToken = '';
    }

    if (storageArr.site === undefined) {
      storageArr.site = '';
    }

    if (storageArr.features === undefined) {
      storageArr.features = '';
    }

    return storageArr;
  }

  // No sync info saved yet.
  return { lastFetchedRevisionId: 0, lastSuccesfulContact: 0, remainingToUpload:0, remainingToDownload: 0, deviceName: '', status: 'Not Available', rollbarToken: '', site: '', features: '' };
};

/**
 * Return saved authorities.
 */
const getSyncInfoAuthorities = function() {
  const storage = localStorage.getItem('syncInfoAuthorities');

  if (!!storage) {
    let storageArr = JSON.parse(storage);
    // Convert values to int.
    storageArr.forEach(function(value, index) {
      value.lastFetchedRevisionId = parseInt(value.lastFetchedRevisionId);
      value.lastSuccesfulContact = parseInt(value.lastSuccesfulContact);
      value.remainingToUpload = parseInt(value.remainingToUpload);
      value.remainingToDownload = parseInt(value.remainingToDownload);
      value.statsCacheHash = value.statsCacheHash;
      value.status = value.status;
      this[index] = value;
    }, storageArr);

    return storageArr;
  }

  return [];
};

/**
 * Return sync speed.
 * See SyncManager.Model.syncSpeed
 */
const getSyncSpeed = function() {
  const storage = localStorage.getItem('syncSpeed');

  if (!!storage) {
    let storageArr = JSON.parse(storage);
    storageArr.idle = parseInt(storageArr.idle);
    storageArr.cycle = parseInt(storageArr.cycle);
    storageArr.offline = parseInt(storageArr.offline);

    return storageArr;
  }

  // Idle time between sync is 10 min.
  // Sync cicle last 50 milliseconds.
  // When offline, we check network state every 30 secons.
  return {idle: (10 * 60 * 1000), cycle: 50, offline: (30 * 1000)};
}

// Start up our Elm app.
var elmApp = Elm.Main.init({
  flags: {
    dbVersion: dbVersion,
    pinCode: localStorage.getItem('pinCode') || '',
    activeServiceWorker: !!navigator.serviceWorker.controller,
    hostname: window.location.hostname,
    activeLanguage: localStorage.getItem('language') || '',
    healthCenterId: localStorage.getItem('healthCenterId') || '',
    villageId: localStorage.getItem('villageId') || '',
    syncInfoGeneral: getSyncInfoGeneral(),
    syncInfoAuthorities: getSyncInfoAuthorities(),
    photoDownloadBatchSize: parseInt(localStorage.getItem('photoDownloadBatchSize')) || (10),
    syncSpeed: getSyncSpeed(),
  }
});

// Request persistent storage, and report whether it was granted.
navigator.storage.persist().then(function(granted) {
  elmApp.ports.persistentStorage.send(granted);
});

// Milliseconds for the specified minutes
function minutesToMillis(minutes) {
  return minutes * 60 * 1000;
}

// Report our quota status.
function reportQuota() {
  navigator.storage.estimate().then(function(quota) {
    elmApp.ports.storageQuota.send(quota);
  });

  if (!!performance.memory) {
    // Firefox doesn't have this property.
    elmApp.ports.memoryQuota.send(performance.memory);
  };
}

// Do it right away.
reportQuota();

// And, then every minute.
setInterval(reportQuota, minutesToMillis(1));

elmApp.ports.cachePinCode.subscribe(function(pinCode) {
  localStorage.setItem('pinCode', pinCode);
});

elmApp.ports.cacheHealthCenter.subscribe(function(healthCenterId) {
  localStorage.setItem('healthCenterId', healthCenterId);
});

elmApp.ports.cacheVillage.subscribe(function(villageId) {
  localStorage.setItem('villageId', villageId);
});

elmApp.ports.setLanguage.subscribe(function(language) {
  // Set the chosen language in the switcher to the local storage.
  localStorage.setItem('language', language);
});

elmApp.ports.scrollToElement.subscribe(function(elementId) {
  waitForElement(elementId, scrollToElement, null);
});

elmApp.ports.getCoordinates.subscribe(function() {
  if ("geolocation" in navigator) {
    navigator.geolocation.getCurrentPosition(
        (position) => {
            const { latitude, longitude } = position.coords;
            const result = {latitude: latitude, longitude: longitude};
            elmApp.ports.coordinates.send(result);
        },
        (error) => {
            console.error("Error fetching location:", error);
        }
    );
  } else {
      console.error("Geolocation is not available.");
  }
});


function scrollToElement(elementId) {
  var element = document.getElementById(elementId);

  if (element) {
    element.scrollIntoView(true);
  }
}

elmApp.ports.refreshPage.subscribe(function() {
  location.reload();
});

/**
 * Set the information about General sync.
 */
elmApp.ports.sendSyncInfoGeneral.subscribe(function(syncInfoGeneral) {
  localStorage.setItem('syncInfoGeneral', JSON.stringify(syncInfoGeneral));
});

/**
 * Set the information about Autohorities sync.
 */
elmApp.ports.sendSyncInfoAuthorities.subscribe(function(syncInfoAuthorities) {
  localStorage.setItem('syncInfoAuthorities', JSON.stringify(syncInfoAuthorities));
});

/**
 * Sets the sync speed.
 */
elmApp.ports.sendSyncSpeed.subscribe(function(syncSpeed) {
  localStorage.setItem('syncSpeed', JSON.stringify(syncSpeed));
});

/**
 * Save Synced data to IndexDB.
 */
elmApp.ports.sendSyncedDataToIndexDb.subscribe(function(info) {

  // Prepare entities for bulk add.
  let entities = [];
  info.data.forEach(function (row) {
    const rowObject = JSON.parse(row);

    let entity = rowObject.entity;
    entity.uuid = rowObject.uuid;
    entity.vid = rowObject.vid;

    if (info.table != 'General') {
      entity.shard = info.shard;
    }

    entities.push(entity);
  })

  var table;
  switch (info.table) {
    case 'Authority':
      table = dbSync.shards;
      break;

    case 'AuthorityStats':
      table = dbSync.statistics;
      break;

    case 'General':
      table = dbSync.nodes;
      break;

    case 'DeferredPhotos':
      table = dbSync.deferredPhotos;
      break;

    default:
      throw info.table + " is an unknown table type.";
  }

  table.bulkPut(entities)
      .then(function() {
          return sendIndexedDbSaveResult('Success', info.table, info.timestamp);
      }).catch(Dexie.BulkError, function (e) {
          return sendIndexedDbSaveResult('Failure', info.table, info.timestamp);
      });

  /**
   * Report that save operation was successful.
   */
  function sendIndexedDbSaveResult(status, table, timestamp) {
    const dataForSend = {
      'status': status,
      'table': table,
      'timestamp': timestamp
    }

    elmApp.ports.savedAtIndexedDb.send(dataForSend);
  }

});

/**
 * Fetch data from IndexDB, and send to Elm.
 *
 * See SyncManager.Model.FetchFromIndexDbQueryType to see possible values.
 */
elmApp.ports.askFromIndexDb.subscribe(function(info) {
  const queryType = info.queryType;

  // Some queries may pass us data.
  const data = info.data;
  switch (queryType) {

    case 'IndexDbQueryUploadPhoto':
      (async () => {

        let result = await dbSync
            .authorityPhotoUploadChanges
            .where('isSynced')
            // IndexDB doesn't index Boolean, so we use an Int to indicate "false".
            .equals(0)
            // We upload photos one by one.
            .limit(1)
            .toArray();

        if (!result[0]) {
          // No photos to upload.
          return sendIndexedDbFetchResult(queryType, {tag: 'Success', result: null});
        }

        const photosUploadCache = "photos-upload";

        const cache = await caches.open(photosUploadCache);

        result.forEach(async function(row, index) {
            const cachedResponse = await cache.match(row.photo);

            if (cachedResponse) {
              const blob = await cachedResponse.blob();
              const formData = new FormData();
              const imageName = 'image-' + getRandom8Digits() + '.jpg';

              formData.set('file', blob, imageName);

              const dataArr = JSON.parse(data);

              const backendUrl = dataArr.backend_url;
              const accessToken = dataArr.access_token;

              const uploadUrl = [
                backendUrl,
                '/api/file-upload?access_token=',
                accessToken,
              ].join('');

              try {
                var response = await fetch(uploadUrl, {
                  method: 'POST',
                  body: formData,
                  // This prevents attaching cookies to request, to prevent
                  // sending authentication cookie, as our desired
                  // authentication method is token.
                  credentials: 'omit'
                });
              }
              catch (e) {
                  // Network error.
                  return sendIndexedDbFetchResult(queryType, {tag: 'Error', error: 'NetworkError', reason: e.toString()});
              }

              if (!response.ok) {
                return sendIndexedDbFetchResult(queryType, {tag: 'Error', error: 'UploadError', reason: row.photo});
              }

              // Response indicated success.
              try {
                var json = await response.json();
              }
              catch (e) {
                // Bad JSON.
                return sendIndexedDbFetchResult(queryType, {tag: 'Error', error: 'BadJson', reason: row.photo});
              }

              const changes = {
                'fileId': parseInt(json.data[0].id),
                'remoteFileName': json.data[0].label,
                'isSynced': 1,
              }

              // Update IndexDb to hold the fileId. As there could have been multiple
              // operations on the same entity, we replace all the photo occurrences.
              // For example, lets say a person's photo was changed, and later also
              // their name. So on the two records there were created on the
              // photoUploadChanges table, the same photo local URL will appear.
              await dbSync.authorityPhotoUploadChanges.where('photo').equals(row.photo).modify(changes);
            }
            else {
              // Photo is registered in IndexDB, but doesn't appear in the cache.
              // For the sync not to get stuck, we set the data of default image instead.
              const changes = {
                'fileId': 5002,
                'remoteFileName': 'image-file',
                'isSynced': 1,
              }

              // Update IndexDb to hold the fileId. As there could have been multiple
              // operations on the same entity, we replace all the photo occurrences.
              // For example, lets say a person's photo was changed, and later also
              // their name. So on the two records there were created on the
              // photoUploadChanges table, the same photo local URL will appear.
              await dbSync.authorityPhotoUploadChanges.where('photo').equals(row.photo).modify(changes);
            }

            return sendIndexedDbFetchResult(queryType, {tag: 'Success', result: row});
        });
      })();
      break;

    case 'IndexDbQueryUploadScreenshot':
      (async () => {

        let result = await dbSync
            .whatsAppUploads
            .where('syncStage')
            // On stage 0, we upload the file to backend.
            .equals(0)
            // We upload screenshots one by one.
            .limit(1)
            .toArray();

        if (!result[0]) {
          // No screenshots to upload.
          return sendIndexedDbFetchResult(queryType, {tag: 'Success', result: null});
        }

        const screenshotsUploadCache = "screenshots-upload";
        const cache = await caches.open(screenshotsUploadCache);

        result.forEach(async function(row, index) {
            const cachedResponse = await cache.match(row.screenshot);

            if (cachedResponse) {
              const blob = await cachedResponse.blob();
              const formData = new FormData();
              const imageName = 'whatsapp-upload-' + getRandom8Digits() + '.png';

              formData.set('file', blob, imageName);

              const dataArr = JSON.parse(data);

              const backendUrl = dataArr.backend_url;
              const accessToken = dataArr.access_token;

              const uploadUrl = [
                backendUrl,
                '/api/file-upload?access_token=',
                accessToken,
              ].join('');

              try {
                var response = await fetch(uploadUrl, {
                  method: 'POST',
                  body: formData,
                  // This prevents attaching cookies to request, to prevent
                  // sending authentication cookie, as our desired
                  // authentication method is token.
                  credentials: 'omit'
                });
              }
              catch (e) {
                  // Network error.
                  return sendIndexedDbFetchResult(queryType, {tag: 'Error', error: 'NetworkError', reason: e.toString()});
              }

              if (!response.ok) {
                return sendIndexedDbFetchResult(queryType, {tag: 'Error', error: 'UploadError', reason: row.screenshot});
              }

              // Response indicated success.
              try {
                var json = await response.json();
              }
              catch (e) {
                // Bad JSON.
                return sendIndexedDbFetchResult(queryType, {tag: 'Error', error: 'BadJson', reason: row.screenshot});
              }

              const changes = {
                'fileId': parseInt(json.data[0].id),
                'syncStage': 1,
              }

              await dbSync.whatsAppUploads.where('screenshot').equals(row.screenshot).modify(changes);
            }
            else {
              // Screenshot is registered in IndexDB, but doesn't appear in the cache.
              // For the sync not to get stuck, we set the data of default image instead.
              const changes = {
                'fileId': 5002,
                'syncStage': 1,
              }

              // Update IndexDb to hold the fileId. As there could have been multiple
              // operations on the same entity, we replace all the screenshot occurrences.
              // For example, lets say a person's screenshot was changed, and later also
              // their name. So on the two records there were created on the
              // screenshotUploadChanges table, the same screenshot local URL will appear.
              await dbSync.authorityPhotoUploadChanges.where('screenshot').equals(row.screenshot).modify(changes);
            }

            return sendIndexedDbFetchResult(queryType, {tag: 'Success', result: row});
        });
      })();
      break;

    case 'IndexDbQueryUploadGeneral':
      (async () => {

        const batchSize = 50;

        let totalEntites = await dbSync
            .nodeChanges
            .where('isSynced')
            .notEqual(1)
            .count();

        if (totalEntites == 0) {
            // No entities for upload found.
            let resultToSend = {
              'entities': [],
              'remaining': 0
            };
            return sendIndexedDbFetchResult(queryType, resultToSend);
        }

        let entitiesResult = await dbSync
            .nodeChanges
            .where('isSynced')
            // Don't include items that were already synced.
            .notEqual(1)
            .limit(batchSize)
            .toArray();

        const resultToSend = {
          'entities': entitiesResult,
          'remaining': totalEntites - entitiesResult.length
        };

        return sendIndexedDbFetchResult(queryType, resultToSend);
      })();
      break;


    case 'IndexDbQueryUploadWhatsApp':
      (async () => {
        const batchSize = 50;

        let totalEntites = await dbSync
            .whatsAppUploads
            .where('syncStage')
            .equals(1)
            .count();

        if (totalEntites == 0) {
          // No entities for upload found.
          let resultToSend = {
            'entities': [],
            'remaining': 0
          };
          return sendIndexedDbFetchResult(queryType, resultToSend);
        }

        let entitiesResult = await dbSync
            .whatsAppUploads
            .where('syncStage')
            .equals(1)
            .limit(batchSize)
            .toArray();

        const resultToSend = {
          'entities': entitiesResult,
          'remaining': totalEntites - entitiesResult.length
        };

        return sendIndexedDbFetchResult(queryType, resultToSend);
      })();
      break;

    case 'IndexDbQueryUploadAuthority':
      let authorityId = data;

      (async () => {

        const batchSize = 50;

        let totalEntites = await dbSync
            .shardChanges
            .where('shard')
            .equals(authorityId)
            .and((item) => { return item.isSynced != 1; })
            .count();

        if (totalEntites == 0) {
          // No entities for upload found.
          let resultToSend = {
            'entities': [],
            'remaining': 0
          };
          return sendIndexedDbFetchResult(queryType, resultToSend);
        }

        let entitiesResult = await dbSync
            .shardChanges
            .where('shard')
            .equals(authorityId)
            .and((item) => { return item.isSynced != 1; })
            .limit(batchSize)
            .toArray();

        let remaining = totalEntites - entitiesResult.length;

        // Query by the localId the `authorityUploadPhotos` to get the matching
        // file ID.
        const localIds = entitiesResult.map(function(row) {
          return row.localId;
        });

        let resultToSend = {
          'entities': entitiesResult,
          'remaining': remaining
        };

        let uploadPhotosResult = await dbSync
            .authorityPhotoUploadChanges
            .where('localId')
            .anyOf(localIds)
            .toArray();

        if (uploadPhotosResult[0]) {
            resultToSend = {
              'entities': entitiesResult,
              'remaining': remaining,
              'uploadPhotos': uploadPhotosResult
            };
        }

        return sendIndexedDbFetchResult(queryType, resultToSend);
      })();
      break;

    case 'IndexDbQueryDeferredPhoto':
      (async () => {

        let result = await dbSync
            .deferredPhotos
            .where('attempts')
            // 2 here means 3 attempts, as it starts from 0.
            .belowOrEqual(2)
            .limit(1)
            // Get attempts sorted, so we won't always grab the same one.
            .sortBy('attempts');

        // If we managed to get non empty result from the query above (limited to 1 result),
        // add to response the number of photos, remaining for download.
        if (result.length == 1) {
            let totalEntites = await dbSync
                .deferredPhotos
                .where('attempts')
                .belowOrEqual(2)
                .count();

            result[0].remaining = totalEntites;
        }

        return sendIndexedDbFetchResult(queryType, result);
      })();
      break;

    case 'IndexDbQueryRemoveDeferredPhoto':
      (async () => {

        // We have nothing to send back. At this point we assume the record
        // was deleted properly. Even if not, and we tried to download it again,
        // eventually the number of attempts will make sure it's never picked
        // up again.
        await dbSync.deferredPhotos.delete(data);
      })();
      break;

    case 'IndexDbQueryUpdateDeferredPhotoAttempts':
      (async () => {

        const dataArr = JSON.parse(data);

        // We don't need to send back the result, as it's an update operation.
        await dbSync.deferredPhotos.update(dataArr.uuid, {'attempts': dataArr.attempts});
      })();
      break;

    case 'IndexDbQueryRemoveUploadPhotos':
      (async () => {

        // We get uuids as a srting, seperated by commas.
        // So, we convert it into array of numbers.
        let uuids = data.split(',').map(Number);

        // We have nothing to send back. At this point we assume the record
        // was deleted properly.
        await dbSync.authorityPhotoUploadChanges
            .where('localId')
            .anyOf(uuids)
            .delete();
      })();
      break;

    case 'IndexDbQueryGetTotalEntriesToUpload':
      (async () => {

        let totalEntites = await dbSync
            .shardChanges
            .count();

        return sendIndexedDbFetchResult(queryType, totalEntites);
      })();
        break;

    // Purpose of this query is to retrieve data tha will help resolving
    // sync incident in case referrenced entity is not recorded. For example,
    // when nutrition height is being recored, but it's encounter is
    // not found and shardChanges table.
    // To solve this, we try to pull the encounter from shards table.
    case 'IndexDbQueryGetShardsEntityByUuid':
      (async () => {
        let result = await dbSync
            .shards
            .where('uuid')
            .equals(data)
            .limit(1)
            .toArray();

        let entities = [result[0]];

        // If resolved entity is an encounter, we fetch the
        // participant it refers to.
        if (entities[0].type.endsWith('_encounter')) {
          // Encounter was resolved. Now resolving participant.
          let uuid = entities[0].individual_participant;
          result = await dbSync
              .shards
              .where('uuid')
              .equals(uuid)
              .limit(1)
              .toArray();

          let participant = result[0];
          if (participant) {
            entities.push(participant);

            // Participant was resolved. Now resolving person.
            uuid = participant.person;
            result = await dbSync
                .shards
                .where('uuid')
                .equals(uuid)
                .limit(1)
                .toArray();

            let person = result[0];
            if (person) {
              entities.push(person);
            }
          }
        }
        else if (entities[0].type == 'individual_participant') {
          // Participant was resolved. Now resolving person.
          let uuid = entities[0].person;
          result = await dbSync
              .shards
              .where('uuid')
              .equals(uuid)
              .limit(1)
              .toArray();

          let person = result[0];
          if (person) {
            entities.push(person);
          }
        }

        if (entities) {
          return sendIndexedDbFetchResult(queryType, JSON.stringify(entities));
        }
      })();
        break;

    default:
      throw queryType + ' is not a known Query type for `askFromIndexDb`';
  }

  /**
   * Prepare and send the result.
   */
  function sendIndexedDbFetchResult(queryType, result) {
    const dataForSend = {
      // Query type should match SyncManager.Model.IndexDbQueryTypeResult
      'queryType': queryType + 'Result',
      'data': result
    }

    elmApp.ports.getFromIndexDb.send(dataForSend);
  }

});

/**
 * Mark local changes are uploaded, so later we could delete them.
 *
 * see elmApp.ports.deleteEntitiesThatWereUploaded
 *
 */
elmApp.ports.deleteEntitiesThatWereUploaded.subscribe(async function(info) {
  const type = info.type_;

  var table;

  switch (type) {
    case 'General':
      table = dbSync.nodeChanges;
      break;

    case 'WhatsApp':
      table = dbSync.whatsAppUploads;
      break;

    case 'Authority':
      table = dbSync.shardChanges;
      break;

    default:
      throw type + " is not a known type for sendLocalIdsForDelete";
  }

  await table
      .where('localId')
      .anyOf(info.localId)
      // For easier debug, we can modify `isSynced` instead, but on production
      // we delete the records.
      // .modify({'isSynced': 1});
      .delete();
});

/**
 * Delete local entities that were already uploaded, and then re-synced.
 */
elmApp.ports.sendLocalIdsForDelete.subscribe(async function(info) {
  const type = info.type_;

  var table;
  var photoUploadTable;

  switch (type) {
    case 'General':
      table = dbSync.nodeChanges;
      break;

    case 'Authority':
      table = dbSync.shardChanges;
      photoUploadTable = dbSync.authorityPhotoUploadChanges;
      break;

    default:
      throw type + " is not a known type for sendLocalIdsForDelete";
  }

  // Find the localIds with matching uuid, that have been already uploaded.
  const result = await table
      .where('uuid')
      .anyOf(info.uuid)
      // Get only the rows there were already uploaded.
      .filter(row => {
        return row.hasOwnProperty('isSynced') && row.isSynced === 1;
      })
      .toArray();

  if (!result[0]) {
    // No matching local changes found.
    return;
  }

  const localIds = result.map(row => row.localId);
  await table.bulkDelete(localIds);

  // Delete also from the photoUploadChanges table.
  if (type == 'Authority') {
    await photoUploadTable.bulkDelete(localIds);
  }

  // Delete photo from cache storage.
  // Note that this is not a safe delete. That is, there could be a case where
  // we have downloaded the entity from the backend, but have not downloaded
  // the photo yet. However, to do it in a safe way, the cleanest way would be
  // to keep track in the backend of the local ID and device that created the
  // photo and send that info back on download upon sync. This will to require
  // more work than we'd like to invest for this use case.
  const row = result[0];
  const cache = await caches.open('photos-upload');
  await cache.delete(row.data.photo);
});


elmApp.ports.makeProgressReportScreenshot.subscribe(function(data) {
  waitForElement('report-content', makeProgressReportScreenshot, data);
});


function makeProgressReportScreenshot(elementId, data) {
  var element = document.getElementById(elementId);

  (async () => {
    const screenshotsUploadCache = 'screenshots-upload';
    const cache = await caches.open(screenshotsUploadCache);

    let totalHeight = 0;
    let children = element.childNodes;

    for (let i = 0; i < children.length; i++) {
      if (children[i].clientHeight == undefined) {
        continue;
      }

      totalHeight += parseInt(children[i].clientHeight);
    }

    // Adding height to make sure we capture complete page.
    // Without this, antenatal reports gets cut off at the top.
    totalHeight += 500;

    const canvas = await html2canvas(element, {
        width: element.clientWidth,
        windowHeight: totalHeight,
        onclone: function(document) {
          var styleSheets = document.styleSheets;
          var promises = [];

          for (var i = 0; i < styleSheets.length; i++) {
            var styleSheet = styleSheets[i];
            if (styleSheet.href) {
              var promise = fetch(styleSheet.href, { cache: 'reload' })
                .then(function(response) {
                  if (response.ok) {
                    return response.text();
                  } else {
                    throw new Error('Failed to fetch stylesheet: ' + response.url);
                  }
                })
                .then(function(cssText) {
                  var style = document.createElement('style');
                  style.textContent = cssText;
                  document.head.appendChild(style);
                })
                .catch(function(error) {
                  console.error('Error fetching stylesheet:', error);
                });

              promises.push(promise);
            }
          }

          return Promise.all(promises);
        }
      });

    canvas.toBlob(async function(blob) {
      const formData = new FormData();
      const imageName = 'whatsapp-upload-' + getRandom8Digits() + '.png';
      formData.set('file', blob, imageName);

      const url = "cache-upload/screenshots/" + Date.now();

      try {
        var response = await fetch(url, {
          method: 'POST',
          body: formData,
          // This prevents attaching cookies to request, to prevent
          // sending authentication cookie, as our desired
          // authentication method is token.
          credentials: 'omit'
        });

        if (response.ok) {
         var json = await response.json();
         var today = new Date();

         var entry = {
             screenshot: json.url,
             person: data.personId,
             date_measured: today.toISOString().split('T')[0],
             language: data.language,
             report_type: data.reportType,
             phone_number: data.phoneNumber,
             syncStage: 0,
             fileId: null
         };

         await dbSync.whatsAppUploads.add(entry);

         reportProgressReportScreenshotResult("success");
        }
        else {
          reportProgressReportScreenshotResult("failure");
        }
      }
      catch (e) {
        reportProgressReportScreenshotResult("failure");
      }
    });
   })();
}

function reportProgressReportScreenshotResult(result) {
  var element = document.getElementById('execution-response');
  if (element) {
    var event = makeCustomEvent("screenshotcomplete", {
      result: result
    });

    element.dispatchEvent(event);
  }
}

function getRandom8Digits () {
  var timestamp = String(performance.timeOrigin + performance.now());
  timestamp = timestamp.replace('.', '');

  return timestamp.slice(timestamp.length - 8);
}

///////////////////////////////////////////////////


// Dropzone.

var dropZone = undefined;

Dropzone.autoDiscover = false;

elmApp.ports.bindDropZone.subscribe(function() {
  waitForElement('dropzone', attachDropzone, null);
});


// Signature Pad.

// https://github.com/szimek/signature_pad
var canvas = undefined;
var signaturePad = undefined;

var signaturePadSelector = 'signature-pad';

elmApp.ports.bindSignaturePad.subscribe(function() {
  waitForElement(signaturePadSelector, attachSignaturePad, null);
});

elmApp.ports.clearSignaturePad.subscribe(function() {
  if (signaturePad === undefined) {
    return;
  }
  signaturePad.clear();
});

elmApp.ports.storeSignature.subscribe(function() {
  if (signaturePad === undefined) {
    return;
  }

  if (signaturePad.isEmpty()) {
    return;
  }

  storeSignatureFromPad();
});

function attachSignaturePad() {
  const wrapper = document.getElementById("signature-pad");
  canvas = wrapper.querySelector("canvas");
  signaturePad = new SignaturePad(canvas, {
    // It's Necessary to use an opaque color when saving image as JPEG;
    // this option can be omitted if only saving as PNG or SVG
    backgroundColor: 'rgb(255, 255, 255)'
  });

  // Adjust canvas coordinate space taking into account pixel ratio,
  // to make it look crisp on mobile devices.
  // This also causes canvas to be cleared.
  function resizeCanvas() {
    // When zoomed out to less than 100%, for some very strange reason,
    // some browsers report devicePixelRatio as less than 1
    // and only part of the canvas is cleared then.
    const ratio =  Math.max(window.devicePixelRatio || 1, 1);

    // This part causes the canvas to be cleared
    canvas.width = canvas.offsetWidth * ratio;
    canvas.height = canvas.offsetHeight * ratio;
    canvas.getContext("2d").scale(ratio, ratio);
    signaturePad.clear();
  }

  window.onresize = resizeCanvas;
  resizeCanvas();
}

function storeSignatureFromPad() {
  (async () => {
    const uploadCache = 'photos-upload';
    const cache = await caches.open(uploadCache);

    signaturePad.canvas.toBlob(async function(blob) {
      const formData = new FormData();
      const imageName = 'signature-' + getRandom8Digits() + '.png';
      formData.set('file', blob, imageName);

      const url = "cache-upload/images/" + Date.now();

      try {
        var response = await fetch(url, {
          method: 'POST',
          body: formData,
          // This prevents attaching cookies to request, to prevent
          // sending authentication cookie, as our desired
          // authentication method is token.
          credentials: 'omit'
        });

        if (response.ok) {
         var json = await response.json();
         reportSignaturePadResult(json.url);
        }
        else {
          // If something goes wrong while storing signature in cache,
          // currently we do nothing.
          // This situation is very rare, and if it does happen, user
          // will most likely repeat the action.
        }
      }
      catch (e) {
        // Something was wrong with storing signature in cache.
        // Take no action (for now).
      }
    });
   })();
}

function reportSignaturePadResult(url) {
  var element = document.getElementById(signaturePadSelector);
  if (element) {
    var event = makeCustomEvent("signaturecomplete", {
      url: url
    });

    element.dispatchEvent(event);
  }
}


// Rollbar.

elmApp.ports.initRollbar.subscribe(function(data) {
  // Generate rollbar config.
  var _rollbarConfig = {
      accessToken: data.token,
      captureUncaught: true,
      captureUnhandledRejections: true,
      payload: {
          environment: 'all',
          client: {
            javascript: {
              code_version: '1.0',
            }
          },
          person: {
            id: data.device,
          }
      }
  };

  // Init rollbar.
  rollbar.init(_rollbarConfig);

  // Send unsynced items from dbErrors table.
  (async () => {

      let result = await dbSync
          .dbErrors
          .where('isSynced')
          // IndexDB doesn't index Boolean, so we use an Int to indicate "false".
          .equals(0)
          .toArray();

      if (!result[0]) {
          // No items to sync.
          return;
      }

      // Send all items.
      let localIds = [];
      result.forEach(function(row) {
          rollbar.log(row.error);
          localIds.push(row.localId);
      })

      // Mark that sent items were synced.
      await dbSync
          .dbErrors
          .where('localId')
          .anyOf(localIds)
          .modify({'isSynced': 1});
  })();

});

elmApp.ports.logByRollbar.subscribe(function(data) {

  (async () => {

      switch (data.source) {
        case 'sw':
        case 'sync':
            let result = await dbSync
                .errorsHash
                .where('hash')
                .equals(data.md5)
                .limit(1)
                .toArray();

            if (result[0]) {
              // Hash exists, indicating that this message was sent alredy.
              return;
            }

            // Send rollbar message.
            rollbar.log(data.message);

            await dbSync.errorsHash.add({ hash: data.md5 });
            break;

        case 'db':
            await dbSync.dbErrors.add({ error: data.message, isSynced: 0 });
            break;
      }

  })();

});


/**
 * Wait for id to appear before invoking related functions.
 */
function waitForElement(id, fn, model, tryCount) {

  // Repeat the timeout maximum 5 times, with increasing
  // intervals (0.5 sec, 1 sec, 2 sec...).
  tryCount = tryCount || 5;
  --tryCount;

  if (tryCount == 0) {
    return;
  }

  setTimeout(function() {
    var selector = "#" + id;
    var element = document.querySelector(selector);

    if (element) {
      fn.call(null, id, model, tryCount);
    }
    else {
      waitForElement(id, fn, model, tryCount);
    }
  }, (5 - tryCount) * 500);
}

function attachDropzone() {
  // We could make this dynamic, if needed
  var selector = "#dropzone";
  var element = document.querySelector(selector);

  if (element) {
    if (element.dropZone) {
      // Bail, since already initialized
      return;
    } else {
      // If we had one, and it's gone away, destroy it.  So, we should
      // only leak one ... it would be even nicer to catch the removal
      // from the DOM, but that's not entirely straightforward. Or,
      // perhaps we'd actually avoid any leak if we just didn't keep a
      // reference? But we necessarily need to keep a reference to the
      // element.
      if (dropZone) dropZone.destroy();
    }
  } else {
    // If we don't find it, do nothing.
    return;
  }

  // TODO: Feed the dictDefaultMessage in as a param, so we can use the
  // translated version.
  dropZone = new Dropzone(selector, {
    url: "cache-upload/images",
    dictDefaultMessage: "Touch here to take a photo, or drop a photo file here.",
    acceptedFiles: "jpg,jpeg,png,gif,image/*",
    capture: 'camera',
    resizeWidth: 600,
    resizeHeight: 800
  });

  dropZone.on('complete', function(file) {
    // We just send the `file` back into Elm, via the view ... Elm can
    // decode the file as it pleases.
    var event = makeCustomEvent("dropzonecomplete", {
      file: file
    });

    element.dispatchEvent(event);

    dropZone.removeFile(file);
  });
}

function makeCustomEvent(eventName, detail) {
  if (typeof(CustomEvent) === 'function') {
    return new CustomEvent(eventName, {
      detail: detail,
      bubbles: true
    });
  } else {
    var event = document.createEvent('CustomEvent');
    event.initCustomEvent(eventName, true, false, detail);
    return event;
  }
}

// Pass along messages from the service worker
navigator.serviceWorker.addEventListener('message', function(event) {
    elmApp.ports.serviceWorkerIn.send(event.data);
});

navigator.serviceWorker.addEventListener('controllerchange', function() {
  // If we detect a controller change, that means we're being managed
  // by a new service worker. In that case, we need to reload the page,
  // since the new service worker may have new HTML or new Javascript
  // for us to execute.
  //
  // It's safe to reload the page here, because we'll only get a new
  // service worker in two cases:
  //
  // - If we had no service worker, so we told the service worker to
  //   skip waiting.
  //
  // - If the user explicitly tells us to proceed with the new version.
  //
  // So, we're not reloading at a moment that should be surprising to
  // the user ... it's either the only thing they can do, or they just
  // told us to do it.
  location.reload();
});

elmApp.ports.serviceWorkerOut.subscribe(function(message) {
  switch (message.tag) {
    case 'Register':
      // Disable the browser's cache for both service-worker.js and any
      // imported scripts.
      var options = {
        updateViaCache: 'none'
      };

      navigator.serviceWorker.register('service-worker.js', options).then(
        function(reg) {
          elmApp.ports.serviceWorkerIn.send({
            tag: 'RegistrationSucceeded'
          });

          if (reg.waiting) {
            elmApp.ports.serviceWorkerIn.send({
              tag: 'SetNewWorker',
              state: reg.waiting.state
            });
          } else if (reg.installing) {
            elmApp.ports.serviceWorkerIn.send({
              tag: 'SetNewWorker',
              state: reg.installing.state
            });
          }

          reg.addEventListener('updatefound', function() {
            // We've got a new service worker that will prepare itself ...
            // how exciting! Let's tell the app the good news.
            var newWorker = reg.installing;

            elmApp.ports.serviceWorkerIn.send({
              tag: 'SetNewWorker',
              state: newWorker.state
            });

            newWorker.addEventListener('statechange', function() {
              elmApp.ports.serviceWorkerIn.send({
                tag: 'SetNewWorker',
                state: newWorker.state
              });
            });
          });
        }).catch(function(error) {
        elmApp.ports.serviceWorkerIn.send({
          tag: 'RegistrationFailed',
          error: JSON.stringify(error)
        });
      });
      break;

    case 'Update':
      // This happens on its own every 24 hours or so, but we can force a
      // check for updates if we like.
      navigator.serviceWorker.getRegistration().then(function(reg) {
        reg.update();
      });
      break;

    case 'SkipWaiting':
      // If we have an installed service worker that is waiting to control
      // pages, tell it to stop waiting. It will claim existing clients
      // (including this one), which in turn will trigger a reload, so we
      // actually get the HTML and Javascript the new service worker will
      // provide. So we make this explicit rather than automatic -- we don't
      // want to reload at some moment the user isn't expecting.
      navigator.serviceWorker.getRegistration().then(function(reg) {
        if (reg.waiting) {
          reg.waiting.postMessage('SkipWaiting');
        }
      });
      break;
  }
});
