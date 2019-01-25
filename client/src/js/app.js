/**
 * Wait for selector to appear before invoking related functions.
 */
function waitForElement(selector, fn, model, tryCount) {

    // Repeat the timeout only maximum 5 times, which sohuld be enough for the
    // element to appear.
    tryCount = tryCount || 5;
    --tryCount;
    if (tryCount == 0) {
        return;
    }

    setTimeout(function() {

        var result = fn.call(null, selector, model, tryCount);
        if (!result) {
            // Element still doesn't exist, so wait some more.
            waitForElement(selector, fn, model, tryCount);
        }
    }, 200);
}

// Normally, you'd want to do this on the server, but there doesn't seem to be
// a mechanism for it on Pantheon, since the request for the app doesn't hit
// the PHP code.
if (location.hostname.endsWith('pantheonsite.io') && location.protocol == 'http:') {
    // This will do a redirect
    location.protocol = 'https:';
}

// The Elm side of the "credentials" mechanism allows us to distinguish between
// credentials for multiple backends. However, we can't really make much use of
// that at the "flags" stage, because on the JS side we don't know what the
// backendUrl is. So, that' probably fine for the moment ... we can think of
// something if we really need it. In any event, using local storage will
// automatically distinguish based on the frontend URL, so that is probably
// enough.
var elmApp = Elm.Main.fullscreen({
    credentials: localStorage.getItem('credentials') || '{}',
    activeServiceWorker: !!navigator.serviceWorker.controller,
    hostname: window.location.hostname,
    activeLanguage : localStorage.getItem('language') || ''
});

elmApp.ports.trySyncing.subscribe(function () {
    // This manually kicks off a sync attempt. Normally, we'll manage this
    // automatically, but it's nice to be able to kick one off directly.
    navigator.serviceWorker.getRegistration().then(function (reg) {
        if (reg.active) {
            reg.active.postMessage('sync');
        }
    });
});

elmApp.ports.cacheCredentials.subscribe(function(params) {
    // The `backendUrl` isn't actually used, for the moment ... we just save
    // the credentials without trying to distinguish amongst backends.
    var backendUrl = params[0];
    var credentials = params[1];

    localStorage.setItem('credentials', credentials);
});

elmApp.ports.cacheEditableSession.subscribe(function(json) {
    // We cache the session and the edits separattely, so that we can treat
    // the session itself as basically immutable, and just keep saving the
    // edits.
    var session = json[0];
    var edits = json[1];

    // For the moment, we'll cache it in the simplest way possible ... we'll
    // see how much more we need to do. We can probably store the JSON as a
    // lump, since we treat it as immutable and don't update it frequently.
    // But we may need to manage quota, or use a different mechanism in order
    // to get more quota.
    localStorage.setItem('session', session);
    localStorage.setItem('edits', edits);

    // TODO: We should catch exceptions ... and report back a real result!
    elmApp.ports.cacheEditableSessionResult.send({});
});

elmApp.ports.fetchEditableSession.subscribe(function () {
    var session = localStorage.getItem('session') || "";
    var edits = localStorage.getItem('edits') || "";

    // TODO: Consider exceptions?
    elmApp.ports.handleEditableSession.send([session, edits]);
});

elmApp.ports.cacheEdits.subscribe(function (json) {
    localStorage.setItem('edits', json);

    // TODO: Consider exceptions ...
    elmApp.ports.cacheEditsResult.send ({});
});

elmApp.ports.deleteEditableSession.subscribe(function () {
    // Delete the session and edits in local storage
    localStorage.setItem('session', "");
    localStorage.setItem('edits', "");
});

elmApp.ports.setLanguage.subscribe(function(language) {
    // Set the choosen language in the switcher to the local storage.
    localStorage.setItem('language', language);
});

// Dropzone.

var dropZone = undefined;

Dropzone.autoDiscover = false;

function bindDropZone () {
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
        console.log("Could not find dropzone div");
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

    dropZone.on('complete', function (file) {
        // We just send the `file` back into Elm, via the view ... Elm can
        // decode the file as it pleases.
        var event = makeCustomEvent("dropzonecomplete", {
            file: file
        });

        element.dispatchEvent(event);

        dropZone.removeFile(file);
    });
}

function makeCustomEvent (eventName, detail) {
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

navigator.serviceWorker.addEventListener('controllerchange', function () {
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

elmApp.ports.serviceWorkerOut.subscribe(function (message) {
  switch (message.tag) {
    case 'Register':
      navigator.serviceWorker.register('service-worker.js').then(function(reg) {
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

        reg.addEventListener('updatefound', function () {
          // We've got a new service worker that will prepare itself ...
          // how exciting! Let's tell the app the good news.
          var newWorker = reg.installing;

          elmApp.ports.serviceWorkerIn.send({
            tag: 'SetNewWorker',
            state: newWorker.state
          });

          newWorker.addEventListener('statechange', function () {
            elmApp.ports.serviceWorkerIn.send({
              tag: 'SetNewWorker',
              state: newWorker.state
            });
          });
        });
      }).catch(function (error) {
        elmApp.ports.serviceWorkerIn.send({
          tag: 'RegistrationFailed',
          error: JSON.stringify(error)
        });
      });
      break;

    case 'Update':
      // This happens on its own every 24 hours or so, but we can force a
      // check for updates if we like.
      navigator.serviceWorker.getRegistration().then(function (reg) {
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
      navigator.serviceWorker.getRegistration().then(function (reg) {
        if (reg.waiting) {
          reg.waiting.postMessage('SkipWaiting');
        }
      });
      break;
  }
});

function withPhotos(func) {
  caches.open("photos").then(func);
}

function updatePhotos () {
  withPhotos(function (cache) {
    cache.keys().then(function (keys) {
      var urls = keys.map(function (request) {
        return request.url;
      });

      elmApp.ports.cacheStorageResponse.send({
        tag: "SetCachedPhotos",
        value: urls
      });
    });
  });
}

elmApp.ports.cacheStorageRequest.subscribe(function (request) {
  switch (request.tag) {
    case 'CachePhotos':
      withPhotos(function (cache) {
        cache.keys().then(function (keys) {
          var existing = keys.map(function (request) {
            return request.url;
          });

          // We'll cache just the ones we don't have already.  This should be
          // fine, since Drupal generates new URLs if the picture chagnes.
          var uncached = request.value.filter(function (url) {
            return existing.indexOf(url) < 0;
          });

          cache.addAll(uncached).then(updatePhotos, function (err) {
            console.log(err);
          });
        });
      });
      break;

    case 'CheckCachedPhotos':
      updatePhotos();
      break;

    case 'ClearCachedPhotos':
      caches.delete("photos").then(function () {
        return caches.delete("photos-upload");
      }).then(updatePhotos);
      break;
  }
});
