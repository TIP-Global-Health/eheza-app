module Backend.NutritionEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Pages.NutritionEncounter.Model exposing (NutritionAssesment(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)
import ZScore.Model
import ZScore.Utils exposing (zScoreWeightForAge)


generatePreviousMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
generatePreviousMeasurementsForChild childId db =
    resolveNutritionParticipantForChild childId db
        |> Maybe.map
            (\participantId ->
                Dict.get participantId db.nutritionEncountersByParticipant
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.toList
                            >> List.filterMap
                                (\( encounterId, encounter ) ->
                                    case Dict.get encounterId db.nutritionMeasurements of
                                        Just (Success data) ->
                                            Just ( encounter.startDate, ( encounterId, data ) )

                                        _ ->
                                            Nothing
                                )
                            -- Most recent date to least recent date.
                            >> List.sortWith
                                (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))
                        )
                    |> RemoteData.withDefault []
            )
        |> Maybe.withDefault []


resolveNutritionParticipantForChild : PersonId -> ModelIndexedDb -> Maybe IndividualEncounterParticipantId
resolveNutritionParticipantForChild id db =
    Dict.get id db.individualParticipantsByPerson
        |> Maybe.withDefault NotAsked
        |> RemoteData.toMaybe
        |> Maybe.andThen
            (Dict.toList
                >> List.filter
                    (\( _, participant ) ->
                        participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                    )
                >> List.head
                >> Maybe.map Tuple.first
            )


generatePreviousValuesForChild : PersonId -> ModelIndexedDb -> PreviousMeasurementsValue
generatePreviousValuesForChild childId db =
    resolveNutritionParticipantForChild childId db
        |> Maybe.map
            (\participantId ->
                let
                    measurementsWithDates =
                        Dict.get participantId db.nutritionEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (Dict.toList
                                    >> List.filterMap
                                        (\( encounterId, encounter ) ->
                                            case Dict.get encounterId db.nutritionMeasurements of
                                                Just (Success data) ->
                                                    Just ( encounter.startDate, data )

                                                _ ->
                                                    Nothing
                                        )
                                    -- Most recent date to least recent date.
                                    >> List.sortWith
                                        (\( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date2 date1)
                                )
                            |> RemoteData.withDefault []

                    previuosHeight =
                        measurementsWithDates
                            |> List.filterMap
                                (\( date, measurements ) ->
                                    measurements.height
                                        |> Maybe.map (\measurement -> ( date, Tuple.second measurement |> .value ))
                                )
                            |> List.head

                    previousMuac =
                        measurementsWithDates
                            |> List.filterMap
                                (\( date, measurements ) ->
                                    measurements.muac
                                        |> Maybe.map (\measurement -> ( date, Tuple.second measurement |> .value ))
                                )
                            |> List.head

                    previousWeight =
                        measurementsWithDates
                            |> List.filterMap
                                (\( date, measurements ) ->
                                    measurements.weight
                                        |> Maybe.map (\measurement -> ( date, Tuple.second measurement |> .value ))
                                )
                            |> List.head
                in
                PreviousMeasurementsValue previuosHeight previousMuac previousWeight
            )
        |> Maybe.withDefault (PreviousMeasurementsValue Nothing Nothing Nothing)


generateNutritionAssesment : NominalDate -> ZScore.Model.Model -> PersonId -> Maybe MuacInCm -> Maybe Float -> Maybe (EverySet.EverySet ChildNutritionSign) -> ModelIndexedDb -> List NutritionAssesment
generateNutritionAssesment currentDate zscores childId muacValue weightValue nutritionValue db =
    let
        child =
            Dict.get childId db.people
                |> Maybe.andThen RemoteData.toMaybe

        -- assesmentByMuac =
        --     muacValue
        --         |> Maybe.andThen
        --             (\muac ->
        --                 if muacSevere muac then
        --                     Just AssesmentAcuteMalnutritionSevere
        --
        --                 else if muacModerate muac then
        --                     Just AssesmentAcuteMalnutritionModerate
        --
        --                 else
        --                     Nothing
        --             )
        --
        -- weightForAgeZScore =
        --     calculateZScoreWeightForAge currentDate zscores child weightValue
        groupWeightMeasurements =
            Dict.get childId db.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.weights
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, weightValueFunc measurement.value ))
                    )
                |> Maybe.withDefault []

        weightValueFunc =
            \(WeightInKg kg) -> kg

        -- previousIndividualWeightMeasurements =
        --     resolvePreviousIndividualValues assembled .weight weightValueFunc
        --
        -- individualWeightMeasurements =
        --     weightValue
        --         |> Maybe.map (\value -> ( currentDate, value ) :: previousIndividualWeightMeasurements)
        --         |> Maybe.withDefault previousIndividualWeightMeasurements
        --
        previousMeasurementsWithDates =
            generatePreviousMeasurementsForChild childId db

        -- allWeigthMeasuements =
        --     groupWeightMeasurements
        --         ++ individualWeightMeasurements
        --         -- Most recent date to least recent date.
        --         |> List.sortWith (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))
        --
        -- assesmentByWeightForAgeZScore =
        --     weightForAgeZScore
        --         |> Maybe.andThen
        --             (\zScore ->
        --                 let
        --                     previousZScore =
        --                         List.Extra.getAt 1 allWeigthMeasuements
        --                             |> Maybe.andThen
        --                                 (\( date, previousWeightValue ) ->
        --                                     calculateZScoreWeightForAge date zscores child (Just previousWeightValue)
        --                                 )
        --                 in
        --                 if zScoreWeightForAgeSevere zScore then
        --                     Just AssesmentUnderweightSevere
        --
        --                 else if zScoreWeightForAgeModerate currentDate child zScore previousZScore then
        --                     Just AssesmentUnderweightModerate
        --
        --                 else
        --                     Nothing
        --             )
        --
        -- -- 3 consecutive weight losses of minimum 0.5kg per visit
        -- assesmentByConsecutiveWeight =
        --     Maybe.andThen
        --         (\age ->
        --             if age < 6 then
        --                 Nothing
        --
        --             else
        --                 let
        --                     fourLatest =
        --                         List.take 4 allWeigthMeasuements
        --                             |> List.map Tuple.second
        --                 in
        --                 if List.length fourLatest < 4 then
        --                     -- There're less than 4 measuremnts, so we can't determine.
        --                     Nothing
        --
        --                 else
        --                     fourLatest
        --                         -- Create a list of diffs between 2 nearstanding values.
        --                         |> List.indexedMap
        --                             (\index weight ->
        --                                 List.Extra.getAt (index + 1) fourLatest
        --                                     |> Maybe.map (\previousWeight -> previousWeight - weight)
        --                             )
        --                         |> List.filterMap identity
        --                         |> (\diffs ->
        --                                 -- Each diff needs to be 0.5 or more
        --                                 if List.all (\diff -> diff >= 0.5) diffs then
        --                                     Just AssesmentConsecutiveWeightLoss
        --
        --                                 else
        --                                     Nothing
        --                            )
        --         )
        --         ageMonths
        --
        -- assementByNutritionSigns =
        --     -- When no oter assement made, we determine it by malnutrition signs.
        --     if List.all isNothing [ assesmentByMuac, assesmentByWeightForAgeZScore, assesmentByConsecutiveWeight ] then
        --         Maybe.andThen
        --             (\age ->
        --                 if age < 6 then
        --                     -- For children under 6 months, we list all danger signs.
        --                     if dangerSignsPresent then
        --                         Just (AssesmentMalnutritionSigns dangerSigns)
        --
        --                     else
        --                         Nothing
        --
        --                 else if List.member Edema dangerSigns then
        --                     -- For children above 6 months, we list only Edema.
        --                     Just (AssesmentMalnutritionSigns [ Edema ])
        --
        --                 else
        --                     Nothing
        --             )
        --             ageMonths
        --
        --     else
        --     -- When Underweight or Acute Malnutrition, we only state with/without danger signs.
        --     if
        --         List.isEmpty dangerSigns
        --     then
        --         Just AssesmentDangerSignsNotPresent
        --
        --     else
        --         Just AssesmentDangerSignsPresent
        --
        -- ageMonths =
        --     ageInMonths currentDate child
        --
        dangerSignsPresent =
            nutritionValue
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter ((/=) NormalChildNutrition)
                        >> List.isEmpty
                        >> not
                    )
                |> Maybe.withDefault False
    in
    []


muacSevere : MuacInCm -> Bool
muacSevere muac =
    muacIndication muac == MuacRed


muacModerate : MuacInCm -> Bool
muacModerate muac =
    muacIndication muac == MuacYellow
