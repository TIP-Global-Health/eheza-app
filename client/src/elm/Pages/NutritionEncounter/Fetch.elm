module Pages.NutritionEncounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch exposing (fetchForChild)
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.nutritionEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            participantId
                |> Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person
    in
    Maybe.Extra.values
        [ Just <| FetchNutritionEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        ]
        ++ (Maybe.map (\personId -> Backend.NutritionEncounter.Fetch.fetchForChild personId db) maybePersonId
                |> Maybe.withDefault []
           )
