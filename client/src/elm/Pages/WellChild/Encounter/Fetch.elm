module Pages.WellChild.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Maybe.Extra
import RemoteData


fetch : WellChildEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.wellChildEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person
    in
    Maybe.Extra.values
        [ Just <| FetchWellChildEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        ]
        ++ (Maybe.map (\personId -> Backend.NutritionEncounter.Fetch.fetch personId db) maybePersonId
                |> Maybe.withDefault []
           )
