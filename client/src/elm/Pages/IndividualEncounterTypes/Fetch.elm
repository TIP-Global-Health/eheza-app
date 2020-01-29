module Pages.IndividualEncounterTypes.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : List MsgIndexedDb
fetch =
    [ FetchSyncData ]
