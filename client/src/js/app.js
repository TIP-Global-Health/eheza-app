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

// The Elm side of the "credentials" mechanism allows us to distinguish between
// credentials for multiple backends. However, we can't really make much use of
// that at the "flags" stage, because on the JS side we don't know what the
// backendUrl is. So, that' probably fine for the moment ... we can think of
// something if we really need it. In any event, using local storage will
// automatically distinguish based on the frontend URL, so that is probably
// enough.
var elmApp = Elm.Main.fullscreen({
    credentials: localStorage.getItem('credentials') || '{}',
    hostname: window.location.hostname
});

elmApp.ports.cacheCredentials.subscribe(function(params) {
    // The `backendUrl` isn't actually used, for the moment ... we just save
    // the credentials without trying to distinguish amongst backends.
    var backendUrl = params[0];
    var credentials = params[1];
    
    localStorage.setItem('credentials', credentials);
});

elmApp.ports.cacheOfflineSession.subscribe(function(json) {
    // For the moment, we'll cache it in the simplest way possible ... we'll
    // see how much more we need to do. We can probably store the JSON as a
    // lump, since we treat it as immutable and don't update it frequently.
    // But we may need to manage quota, or use a different mechanism in order
    // to get more quota.
    localStorage.setItem('offlineSession', json);

    // TODO: We should catch exceptions ... and report back a real result!
    elmApp.ports.cacheOfflineSessionResult.send({});
});

elmApp.ports.fetchOfflineSession.subscribe(function () {
    var session = localStorage.getItem('offlineSession');

    // TODO: Consider exceptions?
    elmApp.ports.handleOfflineSession.send(session || "");
});

/**
 * Port the 'Pusher' events names into our Elm's app.
 */
elmApp.ports.pusherKey.subscribe(function(appKey) {
    var pusher = new Pusher(appKey[0], {
        cluster: appKey[1]
    });

    var channelName = 'general';

    if (!pusher.channel(channelName)) {
        var channel = pusher.subscribe(channelName);

        var eventNames = appKey[2];

        eventNames.forEach(function(eventName) {
            channel.bind(eventName, function(data) {
                // We wrap the data with some information which will
                // help us dispatch it on the Elm side
                var event = {
                    eventType: eventName,
                    data: data
                };
                elmApp.ports.pusherItemMessages.send(event);
            });
        });
    }
});

Offline.on('down', function() {
    elmApp.ports.offline.send(true);
});

Offline.on('up', function() {
    elmApp.ports.offline.send(false);
});

// Dropzone.
var dropZone = undefined;

/*
elmApp.ports.dropzoneConfig.subscribe(function(config) {
    waitForElement('.dropzone', attachDropzone, config);
});

elmApp.ports.dropzoneReset.subscribe(function() {
  if (typeof dropZone != 'undefined') {
      dropZone.removeAllFiles(true);
  }
})

function attachDropzone(selector, config) {
    // Validate dropzone should be active.
    if (!config.active) {
        if (!!dropZone) {
            dropZone.destroy();
        }

        dropZone = undefined;

        // DropZone.destory() doesn't clean it's HTML. So in order not to
        // confuse the Virtual dom we do it ourself.
        var classNames = ['.dz-default', '.dz-preview'];
        classNames.forEach(function(className) {
            var element = document.querySelector(className);
            if (!!element) {
                element.parentNode.removeChild(element);
            }
        });

    }

    var element = document.querySelector(selector);
    if (!element) {
        // Element doesn't exist yet.
        return false;
    }

    if (!!dropZone) {

        // Check if we need to remove files.
        if (config.status == "Done") {
            // Remove all files, even the ones being currently uploaded.
            dropZone.removeAllFiles(true);
        }

        // Widgets were already attached once.
        return true;
    }

    var accessToken = localStorage.getItem('accessToken');
    if (!accessToken) {
        // Access token is must for the requests.
        return false;
    }

    // Set the backend url with the access token.
    var url = config.backendUrl + '/api/file-upload?access_token=' + accessToken;

    dropZone = new Dropzone(selector, {
        dictDefaultMessage: config.defaultMessage,
        maxFiles: 1,
        url: url
    });

    dropZone.on('complete', function(file) {
        if (!file.accepted) {
            // File was not uploaded.
            return;
        }

        if (file.xhr.status !== 200) {
            return;
        }

        var response = JSON.parse(file.xhr.response);

        // Get the file ID, and send it to Elm.
        var id = parseInt(response.data[0]['id']);
        elmApp.ports.dropzoneUploadedFile.send(id);
    });
}
*/
