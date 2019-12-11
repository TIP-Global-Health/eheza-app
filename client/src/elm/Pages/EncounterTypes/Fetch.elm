module Pages.EncounterTypes.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : PersonId -> List MsgIndexedDb
fetch id =
    [ FetchSyncData
    , FetchPerson id
    ]
