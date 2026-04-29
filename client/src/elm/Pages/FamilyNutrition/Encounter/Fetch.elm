module Pages.FamilyNutrition.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.FamilyNutritionEncounter.Fetch
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import RemoteData


fetch : FamilyNutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.familyNutritionEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            Maybe.andThen (\id_ -> Dict.get id_ db.familyParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person
    in
    Maybe.Extra.values
        [ Just <| FetchFamilyNutritionEncounter id
        , Maybe.map FetchFamilyEncounterParticipant participantId
        ]
        ++ (Maybe.map (\personId -> Backend.FamilyNutritionEncounter.Fetch.fetch personId db) maybePersonId
                |> Maybe.withDefault []
           )
