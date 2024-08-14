module Pages.Nutrition.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Maybe.Extra
import RemoteData


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.nutritionEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person
    in
    Maybe.Extra.values
        [ Just <| FetchNutritionEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        ]
        ++ (Maybe.map (\personId -> Backend.NutritionEncounter.Fetch.fetch personId db) maybePersonId
                |> Maybe.withDefault []
           )
