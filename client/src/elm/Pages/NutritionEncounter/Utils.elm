module Pages.NutritionEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (..)
import Backend.Person.Utils exposing (ageInMonths)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NutritionActivity.Utils
    exposing
        ( calculateZScoreWeightForAge
        , muacModerate
        , muacSevere
        , zScoreWeightForAgeModerate
        , zScoreWeightForAgeSevere
        )
import Pages.NutritionEncounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import ZScore.Model


generatePreviousMeasurements : NutritionEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> WebData (List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) ))
generatePreviousMeasurements currentEncounterId participantId db =
    Dict.get participantId db.nutritionEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.map
            (Dict.toList
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter.
                        if encounterId == currentEncounterId then
                            Nothing

                        else
                            case Dict.get encounterId db.nutritionMeasurements of
                                Just (Success data) ->
                                    Just ( encounter.startDate, ( encounterId, data ) )

                                _ ->
                                    Nothing
                    )
                -- Most recent date to least recent date.
                >> List.sortWith
                    (\( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date2 date1)
            )


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
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> RemoteData.withDefault []

        previousMeasurements =
            List.map Tuple.second previousMeasurementsWithDates
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


generateNutritionAssesment : NominalDate -> ZScore.Model.Model -> AssembledData -> List NutritionAssesment
generateNutritionAssesment currentDate zscores assembled =
    let
        measurements =
            assembled.measurements

        child =
            assembled.person

        muacValue =
            measurements.muac
                |> Maybe.map (Tuple.second >> .value)

        assesmentByMuac =
            muacValue
                |> Maybe.andThen
                    (\muac ->
                        if muacSevere muac then
                            Just AssesmentAcuteMalnutritionSevere

                        else if muacModerate muac then
                            Just AssesmentAcuteMalnutritionModerate

                        else
                            Nothing
                    )

        weightValue =
            measurements.weight
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> (\(WeightInKg weight) -> weight)
                    )

        weightForAgeZScore =
            calculateZScoreWeightForAge currentDate zscores child weightValue

        assesmentByWeightForAgeZScore =
            weightForAgeZScore
                |> Maybe.andThen
                    (\zScore ->
                        if zScoreWeightForAgeSevere zScore then
                            Just AssesmentUnderweightSevere

                        else if zScoreWeightForAgeModerate currentDate child zScore then
                            Just AssesmentUnderweightModerate

                        else
                            Nothing
                    )

        assementByNutritionSigns =
            -- When no Underweight /  Acute Malnutrition, we determine assesment by malnutrition signs.
            if isNothing assesmentByMuac && isNothing assesmentByWeightForAgeZScore then
                ageInMonths currentDate child
                    |> Maybe.andThen
                        (\ageMonths ->
                            if ageMonths < 6 then
                                -- For children under 6 months, we list all danger signs.
                                if dangerSignsPresent then
                                    Just (AssesmentMalnutritionSigns dangerSigns)

                                else
                                    Nothing

                            else if List.member Edema dangerSigns then
                                -- For children above 6 months, we list only Edema.
                                Just (AssesmentMalnutritionSigns [ Edema ])

                            else
                                Nothing
                        )

            else
            -- When Underweight or Acute Malnutrition, we only state with/without danger signs.
            if
                List.isEmpty dangerSigns
            then
                Just AssesmentDangerSignsNotPresent

            else
                Just AssesmentDangerSignsPresent

        dangerSigns =
            measurements.nutrition
                |> Maybe.map (Tuple.second >> .value >> EverySet.toList >> List.filter ((/=) NormalChildNutrition))
                |> Maybe.withDefault []

        dangerSignsPresent =
            not <| List.isEmpty dangerSigns
    in
    [ assesmentByMuac, assesmentByWeightForAgeZScore, assementByNutritionSigns ]
        |> List.filterMap identity
