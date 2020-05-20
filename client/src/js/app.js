// Normally, you'd want to do this on the server, but there doesn't seem to be
// a mechanism for it on Pantheon, since the request for the app doesn't hit
// the PHP code.
if ((location.hostname.endsWith('pantheonsite.io') || (location.hostname ===
    '***REMOVED***')) && location.protocol == 'http:') {
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



// Start up our Elm app.
var elmApp = Elm.Main.init({
  flags: {
    pinCode: localStorage.getItem('pinCode') || '',
    activeServiceWorker: !!navigator.serviceWorker.controller,
    hostname: window.location.hostname,
    activeLanguage: localStorage.getItem('language') || '',
    healthCenterId: localStorage.getItem('healthCenterId') || '',
    villageId: localStorage.getItem('villageId') || '',
    lastFetchedRevisionIdGeneral: localStorage.getItem('lastFetchedRevisionIdGeneral') || 0,
  }
});

// Request persistent storage, and report whether it was granted.
navigator.storage.persist().then(function(granted) {
  elmApp.ports.persistentStorage.send(granted);

  (async () => {
    console.log('calling');
    const result = await dbSync.nodes.add({
      uuid: 'Camilla' + Date().toString(),
      type: 'person',
      vid: 123,
    });

    await dbSync
        .nodes
        .where('uuid')
        .equals('00726ed9-32ce-5008-b69a-538d6bc2f00e')
        .toArray(function(data) {console.log(data)});

    console.log(result);
  })();

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

  elmApp.ports.memoryQuota.send(performance.memory);
}

// Do it right away.
reportQuota();

// And, then every minute.
setInterval(reportQuota, minutesToMillis(1));



// Kick off a sync. If we're offline, the browser's sync mechanism will wait
// until we're online.
function trySyncing() {
  // navigator.serviceWorker.ready.then(function(reg) {
  //   return reg.sync.register('sync').catch(function() {
  //     // Try a message instead.
  //     reg.active.postMessage('sync');
  //   });
  // });
}

// Do it on launch.
trySyncing();

// And try a sync every 5 minutes. In future, we may react to Pusher messages
// instead of polling.
setInterval(trySyncing, minutesToMillis(5));

// And allow a manual attempt.
elmApp.ports.trySyncing.subscribe(trySyncing);


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
  var element = document.getElementById(elementId);

  if (element) {
    element.scrollIntoView(true);
  }
});

// Dropzone.

var dropZone = undefined;

Dropzone.autoDiscover = false;

elmApp.ports.bindDropZone.subscribe(function() {
  waitForElement('dropzone', attachDropzone, null);
});

/**
 * Wait for id to appear before invoking related functions.
 */
function waitForElement(id, fn, model, tryCount) {

  // Repeat the timeout only maximum 5 times, which sohuld be enough for the
  // element to appear.
  tryCount = tryCount || 5;
  --tryCount;
  if (tryCount == 0) {
    return;
  }

  setTimeout(function() {

    var result = fn.call(null, id, model, tryCount);
    if (!result) {
      // Element still doesn't exist, so wait some more.
      waitForElement(id, fn, model, tryCount);
    }
  }, 50);
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
    resizeWidth: 800,
    resizeHeight: 800,
    resizeMethod: "contain",
    acceptedFiles: "jpg,jpeg,png,gif,image/*"
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
  // elmApp.ports.serviceWorkerIn.send(event.data);
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
