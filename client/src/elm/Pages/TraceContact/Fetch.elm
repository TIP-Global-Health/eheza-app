module Pages.TraceContact.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData)


fetch : AcuteIllnessTraceContactId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        traceContact =
            Dict.get id db.traceContacts
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .value

        fetchTracePersonMsg =
            Maybe.map (.personId >> FetchPerson >> List.singleton)
                traceContact
                |> Maybe.withDefault []
    in
    FetchTraceContact id :: fetchTracePersonMsg
