module Pages.TraceContact.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))


fetch : AcuteIllnessTraceContactId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    [ FetchTraceContact id ]
