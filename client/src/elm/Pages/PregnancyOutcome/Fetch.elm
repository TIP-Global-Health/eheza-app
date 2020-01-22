module Pages.PregnancyOutcome.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : PrenatalParticipantId -> List MsgIndexedDb
fetch id =
    [ FetchSyncData
    ]
