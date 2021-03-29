module Backend.NutritionEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isNothing)
import Pages.NutritionEncounter.Model exposing (NutritionAssesment(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)
import Utils.NominalDate exposing (diffDays)
import ZScore.Model exposing (Kilograms(..))
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


generateNutritionAssesment :
    NominalDate
    -> ZScore.Model.Model
    -> PersonId
    -> Maybe MuacInCm
    -> Maybe Float
    -> Maybe (EverySet.EverySet ChildNutritionSign)
    -> ModelIndexedDb
    -> List NutritionAssesment
generateNutritionAssesment currentDate zscores childId muacValue weightValue nutritionValue db =
    let
        child =
            Dict.get childId db.people
                |> Maybe.andThen RemoteData.toMaybe

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

        weightValueFunc =
            \(WeightInKg kg) -> kg

        individualMeasurements =
            generatePreviousMeasurementsForChild childId db

        individualWeightMeasurements =
            resolvePreviousIndividualValues individualMeasurements .weight weightValueFunc

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

        allWeightMeasuements =
            groupWeightMeasurements
                ++ individualWeightMeasurements
                |> List.sortWith (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))

        weightForAgeZScore =
            Maybe.map (\child_ -> calculateZScoreWeightForAge currentDate zscores child_ weightValue) child

        assesmentByWeightForAgeZScore =
            Maybe.andThen
                (\child_ ->
                    calculateZScoreWeightForAge currentDate zscores child_ weightValue
                        |> Maybe.andThen
                            (\zScore ->
                                let
                                    previousZScore =
                                        List.Extra.getAt 1 allWeightMeasuements
                                            |> Maybe.andThen
                                                (\( date, previousWeightValue ) ->
                                                    calculateZScoreWeightForAge date zscores child_ (Just previousWeightValue)
                                                )
                                in
                                if zScoreWeightForAgeSevere zScore then
                                    Just AssesmentUnderweightSevere

                                else if zScoreWeightForAgeModerate currentDate child_ zScore previousZScore then
                                    Just AssesmentUnderweightModerate

                                else
                                    Nothing
                            )
                )
                child

        -- 3 consecutive weight losses of minimum 0.5kg per visit
        assesmentByConsecutiveWeight =
            Maybe.andThen
                (\age ->
                    if age < 6 then
                        Nothing

                    else
                        let
                            fourLatest =
                                List.take 4 allWeightMeasuements
                                    |> List.map Tuple.second
                        in
                        if List.length fourLatest < 4 then
                            -- There're less than 4 measuremnts, so we can't determine.
                            Nothing

                        else
                            fourLatest
                                -- Create a list of diffs between 2 nearstanding values.
                                |> List.indexedMap
                                    (\index weight ->
                                        List.Extra.getAt (index + 1) fourLatest
                                            |> Maybe.map (\previousWeight -> previousWeight - weight)
                                    )
                                |> List.filterMap identity
                                |> (\diffs ->
                                        -- Each diff needs to be 0.5 or more
                                        if List.all (\diff -> diff >= 0.5) diffs then
                                            Just AssesmentConsecutiveWeightLoss

                                        else
                                            Nothing
                                   )
                )
                ageMonths

        assementByNutritionSigns =
            -- When no oter assement made, we determine it by malnutrition signs.
            if List.all isNothing [ assesmentByMuac, assesmentByWeightForAgeZScore, assesmentByConsecutiveWeight ] then
                Maybe.andThen
                    (\age ->
                        if age < 6 then
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
                    ageMonths

            else
            -- When Underweight or Acute Malnutrition, we only state with/without danger signs.
            if
                List.isEmpty dangerSigns
            then
                Just AssesmentDangerSignsNotPresent

            else
                Just AssesmentDangerSignsPresent

        ageMonths =
            Maybe.andThen (ageInMonths currentDate) child

        dangerSignsPresent =
            not <| List.isEmpty dangerSigns

        dangerSigns =
            nutritionValue
                |> Maybe.map
                    (EverySet.toList
                        >> List.filter ((/=) NormalChildNutrition)
                    )
                |> Maybe.withDefault []
    in
    [ assesmentByMuac, assesmentByWeightForAgeZScore, assesmentByConsecutiveWeight, assementByNutritionSigns ]
        |> List.filterMap identity


resolvePreviousIndividualValues :
    List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a ))
    -> (a -> b)
    -> List ( NominalDate, b )
resolvePreviousIndividualValues previousMeasurementsWithDates measurementFunc valueFunc =
    previousMeasurementsWithDates
        |> List.filterMap
            (\( date, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.map
                        (\measurement ->
                            ( date, Tuple.second measurement |> .value |> valueFunc )
                        )
            )
        |> List.reverse


calculateZScoreWeightForAge : NominalDate -> ZScore.Model.Model -> Person -> Maybe Float -> Maybe Float
calculateZScoreWeightForAge currentDate zscores person maybeWeight =
    let
        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate
    in
    maybeWeight
        |> Maybe.andThen
            (\weight ->
                Maybe.andThen
                    (\ageInDays ->
                        zScoreWeightForAge zscores ageInDays person.gender (Kilograms weight)
                    )
                    maybeAgeInDays
            )


zScoreWeightForAgeModerate : NominalDate -> Person -> Float -> Maybe Float -> Bool
zScoreWeightForAgeModerate currentDate child zScore previousZScore =
    ageInMonths currentDate child
        |> Maybe.map
            (\ageMonths ->
                let
                    isModerate score =
                        score > -3 && score <= -2
                in
                if ageMonths < 6 then
                    -- When child is 0-6 months we examine zScore of curremt encounter.
                    isModerate zScore

                else
                    -- Otherwise, we examine zScore of current and previous encounters.
                    previousZScore
                        |> Maybe.map (\zScorePrevious -> isModerate zScore && isModerate zScorePrevious)
                        |> Maybe.withDefault False
            )
        |> Maybe.withDefault False


zScoreWeightForAgeSevere : Float -> Bool
zScoreWeightForAgeSevere zScore =
    zScore <= -3


muacSevere : MuacInCm -> Bool
muacSevere muac =
    muacIndication muac == MuacRed


muacModerate : MuacInCm -> Bool
muacModerate muac =
    muacIndication muac == MuacYellow
