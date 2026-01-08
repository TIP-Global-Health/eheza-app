module Pages.HealthyStart.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import RemoteData


fetch : HealthyStartEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.healthyStartEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchHealthyStartEncountersForParticipant participantId
        , Just <| FetchHealthyStartEncounter id
        ]
