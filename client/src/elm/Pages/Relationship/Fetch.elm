module Pages.Relationship.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))


fetch : PersonId -> PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id1 id2 db =
    -- FetchRelationshipsForPerson gets both sides, so we don't
    -- need to do it twice.
    [ FetchRelationshipsForPerson id1
    , FetchParticipantsForPerson id1
    , FetchPerson id1
    , FetchPerson id2
    , FetchClinics
    ]
