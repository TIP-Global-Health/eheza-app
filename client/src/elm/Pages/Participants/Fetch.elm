module Pages.Participants.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : SessionId -> List MsgIndexedDb
fetch sessionId =
    [ FetchEditableSessionMeasurements sessionId
    , FetchEditableSessionCheckedIn sessionId
    , FetchEditableSessionSummaryByParticipant sessionId
    ]
