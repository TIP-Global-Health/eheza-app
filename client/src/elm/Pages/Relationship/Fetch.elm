module Pages.Relationship.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : PersonId -> PersonId -> List MsgIndexedDb
fetch id1 id2 =
    -- FetchRelationshipsForPerson gets both sides, so we don't
    -- need to do it twice.
    [ FetchRelationshipsForPerson id1
    , FetchParticipantsForPerson id1
    , FetchPerson id1
    , FetchPerson id2
    , FetchClinics
    ]
