module Pages.Nutrition.Encounter.Utils exposing (generateAssembledData, generatePreviousMeasurements)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getNutritionEncountersForParticipant)
import Backend.Utils exposing (resolveIndividualParticipantWithDuplicates)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Nutrition.Encounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)


generateAssembledData : NutritionEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.nutritionEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.nutritionMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
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
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements (Just id) (resolveIndividualParticipantWithDuplicates encounter_.participant db) db)
                |> Maybe.withDefault []
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


generatePreviousMeasurements :
    Maybe NutritionEncounterId
    -> List IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
generatePreviousMeasurements =
    Backend.Measurement.Utils.generatePreviousMeasurements getNutritionEncountersForParticipant .nutritionMeasurements
