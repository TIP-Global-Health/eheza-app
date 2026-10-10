// Reads backend sync items (a JSON array) on stdin, and prints, keyed by UUID,
// the incident details a device holding each item would send, or why the
// device could not decode it.
const { Elm } = require('./worker.js');

// Copied from client/src/js/app.js.
function gatherWords (text) {
  return (text || '').split(/\s+/).flatMap(function (word) {
    if (word) {
      return [word.toLowerCase()];
    } else {
      return [];
    }
  });
}

let input = '';
process.stdin.on('data', (chunk) => { input += chunk; });
process.stdin.on('end', () => {
  const app = Elm.SyncIncidentWorker.init({ flags: JSON.parse(input) });
  app.ports.output.subscribe((results) => {
    if (results.fatal) {
      console.log(JSON.stringify({ fatal: results.fatal }));
      return;
    }

    const out = {};
    results.forEach((result) => {
      if (result.decodeError) {
        out[result.uuid] = { decodeError: result.decodeError };
        return;
      }

      // As sendSyncedDataToIndexDb in app.js stores the row.
      const rowObject = JSON.parse(result.row);
      let entity = rowObject.entity;
      entity.uuid = rowObject.uuid;
      entity.vid = rowObject.vid;
      entity.shard = 'sync-incident-check';

      // As the dbSync.shards 'creating' hook in app.js.
      if (entity.type === 'person' && typeof entity.label == 'string') {
        entity.name_search = gatherWords(entity.label);
      }

      // As IndexDbQueryGetShardsEntityByUuid in app.js builds the details.
      out[result.uuid] = { details: JSON.stringify([entity]) };
    });
    console.log(JSON.stringify(out));
  });
});
