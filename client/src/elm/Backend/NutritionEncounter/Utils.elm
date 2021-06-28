module Backend.NutritionEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isNothing)
import Pages.Utils exposing (ifEverySetEmpty)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)
import Utils.NominalDate exposing (diffDays)
import ZScore.Model exposing (Kilograms(..))
import ZScore.Utils exposing (zScoreWeightForAge)


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

        allWeightMeasuements =
            resolveNutritionWeightMeasurementsForChild childId db

        assesmentByWeightForAgeZScore =
            Maybe.andThen
                (\child_ ->
                    calculateZScoreWeightForAge currentDate zscores child_ weightValue
                        |> Maybe.andThen
                            (\zScore ->
                                if zScoreWeightForAgeSevere zScore then
                                    Just AssesmentUnderweightSevere

                                else
                                    let
                                        previousZScore =
                                            List.Extra.getAt 1 allWeightMeasuements
                                                |> Maybe.andThen
                                                    (\( date, previousWeightValue ) ->
                                                        calculateZScoreWeightForAge date zscores child_ (Just previousWeightValue)
                                                    )
                                    in
                                    if zScoreWeightForAgeModerate currentDate child_ zScore previousZScore then
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
                                |> Maybe.Extra.values
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
        |> Maybe.Extra.values


generateIndividualNutritionMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
generateIndividualNutritionMeasurementsForChild childId db =
    resolveIndividualParticipantForPerson childId NutritionEncounter db
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
                            >> List.sortWith sortTuplesByDateDesc
                        )
                    |> RemoteData.withDefault []
            )
        |> Maybe.withDefault []


generateIndividualWellChildMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
generateIndividualWellChildMeasurementsForChild childId db =
    resolveIndividualParticipantForPerson childId WellChildEncounter db
        |> Maybe.map
            (\participantId ->
                Dict.get participantId db.wellChildEncountersByParticipant
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.toList
                            >> List.filterMap
                                (\( encounterId, encounter ) ->
                                    case Dict.get encounterId db.wellChildMeasurements of
                                        Just (Success data) ->
                                            Just ( encounter.startDate, ( encounterId, data ) )

                                        _ ->
                                            Nothing
                                )
                            -- Most recent date to least recent date.
                            >> List.sortWith sortTuplesByDateDesc
                        )
                    |> RemoteData.withDefault []
            )
        |> Maybe.withDefault []


generatePreviousIndividualValuesForChild : PersonId -> ModelIndexedDb -> PreviousMeasurementsValue
generatePreviousIndividualValuesForChild childId db =
    let
        previousValuesByEncounterType encounterType encountersByParticipantFunc measurementsFunc =
            resolveIndividualParticipantForPerson childId NutritionEncounter db
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

                            getPreviousValue getValueFunc toFloatFunc =
                                measurementsWithDates
                                    |> List.filterMap
                                        (\( date, measurements ) ->
                                            getValueFunc measurements
                                                |> Maybe.map
                                                    (\measurement ->
                                                        ( date
                                                        , Tuple.second measurement
                                                            |> .value
                                                            |> toFloatFunc
                                                        )
                                                    )
                                        )
                                    |> List.head

                            previuosHeight =
                                getPreviousValue .height (\(HeightInCm cm) -> cm)

                            previousMuac =
                                getPreviousValue .muac (\(MuacInCm cm) -> cm)

                            previousWeight =
                                getPreviousValue .weight (\(WeightInKg kg) -> kg)
                        in
                        PreviousMeasurementsValue previuosHeight previousMuac previousWeight
                    )
                |> Maybe.withDefault (PreviousMeasurementsValue Nothing Nothing Nothing)

        previousNutritionValues =
            previousValuesByEncounterType NutritionEncounter .nutritionEncountersByParticipant .nutritionMeasurements

        previousWellChildValues =
            previousValuesByEncounterType WellChildEncounter .wellChildEncountersByParticipant .wellChildMeasurements

        height =
            resolvePreviousValueInCommonContext previousNutritionValues.height previousWellChildValues.height

        muac =
            resolvePreviousValueInCommonContext previousNutritionValues.muac previousWellChildValues.muac

        weight =
            resolvePreviousValueInCommonContext previousNutritionValues.weight previousWellChildValues.weight
    in
    PreviousMeasurementsValue height muac weight


{-| Here we get a Float measurement value with it's date\_measured, from group and individual contexts.
We return the most recent value, or Nothing, if both provided parameters were Nothing.
-}
resolvePreviousValueInCommonContext : Maybe ( NominalDate, Float ) -> Maybe ( NominalDate, Float ) -> Maybe ( NominalDate, Float )
resolvePreviousValueInCommonContext value1 value2 =
    case value1 of
        Just ( v1Date, v1Value ) ->
            case value2 of
                Just ( v2Date, v2Value ) ->
                    case Gizra.NominalDate.compare v1Date v2Date of
                        GT ->
                            value1

                        _ ->
                            value2

                Nothing ->
                    value1

        Nothing ->
            value2


resolveAllWeightMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, Float )
resolveAllWeightMeasurementsForChild childId db =
    let
        nutritionWeights =
            resolveNutritionWeightMeasurementsForChild childId db

        wellChildWeights =
            resolveWellChildWeightMeasurementsForChild childId db
    in
    nutritionWeights
        ++ wellChildWeights
        |> List.sortWith sortTuplesByDateDesc


resolveNutritionWeightMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, Float )
resolveNutritionWeightMeasurementsForChild childId db =
    let
        individualMeasurements =
            generateIndividualNutritionMeasurementsForChild childId db

        individualWeightMeasurements =
            resolveIndividualNutritionValues individualMeasurements .weight weightValueFunc

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
    in
    groupWeightMeasurements
        ++ individualWeightMeasurements
        |> List.sortWith sortTuplesByDateDesc


resolveIndividualNutritionValues :
    List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a ))
    -> (a -> b)
    -> List ( NominalDate, b )
resolveIndividualNutritionValues measurementsWithDates measurementFunc valueFunc =
    measurementsWithDates
        |> List.filterMap
            (\( date, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.map
                        (\measurement ->
                            ( date, Tuple.second measurement |> .value |> valueFunc )
                        )
            )
        |> List.reverse


resolveWellChildWeightMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, Float )
resolveWellChildWeightMeasurementsForChild childId db =
    let
        individualMeasurements =
            generateIndividualWellChildMeasurementsForChild childId db
    in
    resolveIndividualWellChildValues individualMeasurements .weight weightValueFunc


resolveIndividualWellChildValues :
    List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> (WellChildMeasurements -> Maybe ( id, WellChildMeasurement a ))
    -> (a -> b)
    -> List ( NominalDate, b )
resolveIndividualWellChildValues measurementsWithDates measurementFunc valueFunc =
    measurementsWithDates
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


nutritionAssesmentForBackend : List NutritionAssesment -> EverySet NutritionAssesment
nutritionAssesmentForBackend assesment =
    EverySet.fromList assesment
        |> ifEverySetEmpty NoNutritionAssesment


weightValueFunc : WeightInKg -> Float
weightValueFunc =
    \(WeightInKg kg) -> kg


sortTuplesByDateDesc : ( NominalDate, a ) -> ( NominalDate, a ) -> Order
sortTuplesByDateDesc m1 m2 =
    Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1)
