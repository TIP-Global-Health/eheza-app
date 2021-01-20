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

// As we defined Dexie's store in app.js, we'll need to redefine tables properties here.
// Since we don't know exactly when the DB will be ready, we define DB placeholder here.
var dbSync = null;
var db = null;

// This defines our URL scheme. A URL will look like
//
// /sw/statistics
var statsUrlRegex = /\/sw\/statistics/;

// If placeholder still indicates tha DB was not initialized,
// initialize it.
self.addEventListener('fetch', event => {
  var url = new URL(event.request.url);
  var matches = statsUrlRegex.exec(url.pathname);

  if (matches) {
    event.respondWith(handleEvent(event));
  }
});

async function handleEvent(event, url, type, uuid) {
    var notFoundResponse = new Response('', {
        status: 404,
        statusText: 'Not Found'
    });

    // If placeholder still indicates tha DB was not initialized,
    // initialize it.
    if (dbSync === null) {
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
      }

      if (event.request.method === 'GET') {
          return index(url);
      }

      // If we get here, respond with a 404
      return notFoundResponse;
});

function index (url) {
    var params = url.searchParams;

    return dbSync.open().catch(databaseError).then(function () {
        var query = dbSync.statistics;
        var getStats = query.toArray();

        return getStats.catch(databaseError).then(function (stats) {

            var body = JSON.stringify({
                offset: 0,
                count: stats.length,
                data: stats
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

    }).catch(sendErrorResponses);
}
