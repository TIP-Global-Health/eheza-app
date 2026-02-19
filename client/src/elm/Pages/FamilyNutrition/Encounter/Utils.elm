module Pages.FamilyNutrition.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.FamilyNutritionEncounter.Utils exposing (getFamilyNutritionEncountersForParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Pages.FamilyNutrition.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


generateAssembledData : FamilyNutritionEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.familyNutritionEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.familyNutritionMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.familyParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            -- @todo:
            -- RemoteData.toMaybe encounter
            --     |> Maybe.map (\encounter_ -> generatePreviousMeasurements (Just id) encounter_.participant db)
            --     |> Maybe.withDefault []
            []
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)



--
--
-- generatePreviousMeasurements :
--     Maybe FamilyNutritionEncounterId
--     -> FamilyEncounterParticipantId
--     -> ModelIndexedDb
--     -> List ( NominalDate, ( FamilyNutritionEncounterId, FamilyNutritionMeasurements ) )
-- generatePreviousMeasurements =
--     Backend.Measurement.Utils.generatePreviousMeasurements getFamilyNutritionEncountersForParticipant .nutritionMeasurements
