module Pages.Activities.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : SessionId -> ( List MsgIndexedDb, List MsgIndexedDb )
fetch sessionId =
    ( []
    , [ FetchEditableSessionMeasurements sessionId
      , FetchEditableSessionCheckedIn sessionId
      , FetchEditableSessionSummaryByActivity sessionId
      ]
    )
