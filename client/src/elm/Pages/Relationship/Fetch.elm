module Pages.Relationship.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator)
import Pages.Person.Fetch exposing (fetchSessionForInitiator)


fetch : PersonId -> PersonId -> Initiator -> List MsgIndexedDb
fetch id1 id2 initiator =
    -- FetchRelationshipsForPerson gets both sides, so we don't
    -- need to do it twice.
    [ FetchRelationshipsForPerson id1
    , FetchParticipantsForPerson id1
    , FetchPerson id1
    , FetchPerson id2
    , FetchClinics
    ]
        ++ fetchSessionForInitiator initiator
