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

var elmApp = Elm.Main.fullscreen({
    accessToken : localStorage.getItem('accessToken') || '',
    hostname : window.location.hostname
});

elmApp.ports.accessTokenPort.subscribe(function(accessToken) {
    localStorage.setItem('accessToken', accessToken);
});

/**
 * Port the 'Pusher' events names into our Elm's app.
 */
elmApp.ports.pusherKey.subscribe(function (appKey) {
  var pusher = new Pusher(appKey[0], {
    cluster: appKey[1]
  });

  var channelName = 'general';

  if (!pusher.channel(channelName)) {
    var channel = pusher.subscribe(channelName);

    var eventNames = appKey[2];

    eventNames.forEach(function (eventName) {
      channel.bind(eventName, function (data) {
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
    elmApp.ports.offline.send (true);
});

Offline.on('up', function() {
    elmApp.ports.offline.send (false);
});

// Dropzone
// @todo: Move to own file.

var dropZone = undefined;

elmApp.ports.activePage.subscribe(function(data) {
  if (data[0].indexOf('Patient') !== 0) {
    // Reset dropzone variable, in case we switch between pages.
    ck = undefined;
    dropZone = undefined;
    return;
  }

  waitForElement('.dropzone', attachDropzone, data);
});

function attachDropzone(selector, data) {
  if (data[0].indexOf('Patient') !== 0) {
    return false;
  }

  var element = document.querySelector(selector);
  if (!element) {
    // Element doesn't exist yet.
    return false;
  }

  if (!!dropZone) {

    // Check if we need to remove files.
    if (data[2] == "Done") {
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
  var url = data[1] + '/api/file-upload?access_token=' + accessToken;

  dropZone = new Dropzone(selector, { url: url});

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
