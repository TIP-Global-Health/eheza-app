module Pages.PatientRecord.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData)


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    [ FetchPerson id ]
