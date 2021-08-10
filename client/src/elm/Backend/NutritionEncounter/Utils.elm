module Backend.NutritionEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (headCircumferenceValueFunc, heightValueFunc, muacIndication, muacValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (..)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Date
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


generateNutritionAssessment :
    NominalDate
    -> ZScore.Model.Model
    -> PersonId
    -> Maybe MuacInCm
    -> Maybe (EverySet.EverySet ChildNutritionSign)
    -> Maybe Float
    -> Bool
    -> ModelIndexedDb
    -> List NutritionAssessment
generateNutritionAssessment currentDate zscores childId muacValue nutritionValue weightValue assessByPreviousWeights db =
    let
        child =
            Dict.get childId db.people
                |> Maybe.andThen RemoteData.toMaybe

        assesmentByMuac =
            muacValue
                |> Maybe.map
                    (\muac ->
                        if muacSevere muac then
                            [ AssesmentAcuteMalnutritionSevere ]

                        else if muacModerate muac then
                            [ AssesmentAcuteMalnutritionModerate ]

                        else
                            []
                    )
                |> Maybe.withDefault []

        assesmentByWeight =
            Maybe.map
                (\child_ ->
                    generateNutritionAssessmentByWeight currentDate zscores childId child_ weightValue assessByPreviousWeights db
                )
                child
                |> Maybe.withDefault []

        assesmentByMuacAndWeight =
            assesmentByMuac ++ assesmentByWeight

        assementByNutritionSigns =
            -- When no oter assement made, we determine it by malnutrition signs.
            if List.isEmpty assesmentByMuacAndWeight then
                Maybe.andThen (ageInMonths currentDate) child
                    |> Maybe.map
                        (\age ->
                            if age < 6 then
                                -- For children under 6 months, we list all danger signs.
                                if dangerSignsPresent then
                                    [ AssesmentMalnutritionSigns dangerSigns ]

                                else
                                    []

                            else if List.member Edema dangerSigns then
                                -- For children above 6 months, we list only Edema.
                                [ AssesmentMalnutritionSigns [ Edema ] ]

                            else
                                []
                        )
                    |> Maybe.withDefault []

            else
            -- When Underweight or Acute Malnutrition, we only state with/without danger signs.
            if
                List.isEmpty dangerSigns
            then
                [ AssesmentDangerSignsNotPresent ]

            else
                [ AssesmentDangerSignsPresent ]

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
    assesmentByMuacAndWeight ++ assementByNutritionSigns


generateNutritionAssessmentByWeight :
    NominalDate
    -> ZScore.Model.Model
    -> PersonId
    -> Person
    -> Maybe Float
    -> Bool
    -> ModelIndexedDb
    -> List NutritionAssessment
generateNutritionAssessmentByWeight currentDate zscores childId child weightValue assessByPreviousWeights db =
    let
        -- We do not want to include here weight
        -- measurements taken at Well Child encounters.
        nutritionWeightMeasuements =
            if assessByPreviousWeights then
                resolveNutritionWeightMeasurementsForChild childId db

            else
                -- We do need to assess by previous weights,
                -- so we don't load previous measurements.
                -- All consiquent calculations will produce Nothing.
                []

        assesmentByWeightForAgeZScore =
            calculateZScoreWeightForAge currentDate zscores child weightValue
                |> Maybe.andThen
                    (\zScore ->
                        if zScoreWeightForAgeSevere zScore then
                            Just AssesmentUnderweightSevere

                        else
                            let
                                previousZScore =
                                    List.Extra.getAt 1 nutritionWeightMeasuements
                                        |> Maybe.andThen
                                            (\( date, previousWeightValue ) ->
                                                calculateZScoreWeightForAge date zscores child (Just previousWeightValue)
                                            )
                            in
                            if zScoreWeightForAgeModerate currentDate child zScore previousZScore then
                                Just AssesmentUnderweightModerate

                            else
                                Nothing
                    )

        -- 3 consecutive weight losses of minimum 0.5kg per visit
        assesmentByConsecutiveWeight =
            ageInMonths currentDate child
                |> Maybe.andThen
                    (\age ->
                        if age < 6 then
                            Nothing

                        else
                            let
                                fourLatest =
                                    List.take 4 nutritionWeightMeasuements
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
    in
    [ assesmentByWeightForAgeZScore, assesmentByConsecutiveWeight ]
        |> Maybe.Extra.values


generateIndividualNutritionMeasurementsForChild : PersonId -> ModelIndexedDb -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
generateIndividualNutritionMeasurementsForChild childId db =
    resolveIndividualParticipantForPerson childId Backend.IndividualEncounterParticipant.Model.NutritionEncounter db
        |> Maybe.map
            (getNutritionEncountersForParticipant db
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
        |> Maybe.withDefault []


getNutritionEncountersForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> List ( NutritionEncounterId, NutritionEncounter )
getNutritionEncountersForParticipant db participantId =
    Dict.get participantId db.nutritionEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
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


resolvePreviousMeasurementsSetForChild :
    PersonId
    -> ModelIndexedDb
    -> PreviousMeasurementsSet
resolvePreviousMeasurementsSetForChild childId db =
    let
        individualNutritionMeasurements =
            generateIndividualNutritionMeasurementsForChild childId db

        nutritionHeights =
            resolveIndividualNutritionValues individualNutritionMeasurements .height heightValueFunc

        nutritionMuacs =
            resolveIndividualNutritionValues individualNutritionMeasurements .muac muacValueFunc

        nutritionWeights =
            resolveIndividualNutritionValues individualNutritionMeasurements .weight weightValueFunc

        individualWellChildMeasurements =
            generateIndividualWellChildMeasurementsForChild childId db

        wellChildHeights =
            resolveIndividualWellChildValues individualWellChildMeasurements .height heightValueFunc

        wellChildMuacs =
            resolveIndividualWellChildValues individualWellChildMeasurements .muac muacValueFunc

        wellChildWeights =
            resolveIndividualWellChildValues individualWellChildMeasurements .weight weightValueFunc

        wellChildHeadCircumferences =
            resolveIndividualWellChildValues individualWellChildMeasurements .headCircumference (.headCircumference >> headCircumferenceValueFunc)

        groupMeasurements =
            Dict.get childId db.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe

        groupHeights =
            groupMeasurements
                |> Maybe.map
                    (.heights
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, heightValueFunc measurement.value ))
                    )
                |> Maybe.withDefault []

        groupMuacs =
            groupMeasurements
                |> Maybe.map
                    (.muacs
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, muacValueFunc measurement.value ))
                    )
                |> Maybe.withDefault []

        groupWeights =
            groupMeasurements
                |> Maybe.map
                    (.weights
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, weightValueFunc measurement.value ))
                    )
                |> Maybe.withDefault []
    in
    { heights = nutritionHeights ++ wellChildHeights ++ groupHeights |> List.sortWith sortTuplesByDateDesc
    , muacs = nutritionMuacs ++ wellChildMuacs ++ groupMuacs |> List.sortWith sortTuplesByDateDesc
    , weights = nutritionWeights ++ wellChildWeights ++ groupWeights |> List.sortWith sortTuplesByDateDesc
    , headCircumferences = wellChildHeadCircumferences
    }


resolvePreviousValuesSetForChild :
    PersonId
    -> ModelIndexedDb
    -> PreviousValuesSet
resolvePreviousValuesSetForChild childId db =
    let
        previousMeasurementsSet =
            resolvePreviousMeasurementsSetForChild childId db

        getLatestValue =
            List.head >> Maybe.map Tuple.second
    in
    PreviousValuesSet
        (getLatestValue previousMeasurementsSet.heights)
        (getLatestValue previousMeasurementsSet.muacs)
        (getLatestValue previousMeasurementsSet.weights)
        (getLatestValue previousMeasurementsSet.headCircumferences)


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
    muacIndication muac == ColorAlertRed


muacModerate : MuacInCm -> Bool
muacModerate muac =
    muacIndication muac == ColorAlertYellow


nutritionAssessmentForBackend : List NutritionAssessment -> EverySet NutritionAssessment
nutritionAssessmentForBackend assesment =
    EverySet.fromList assesment
        |> ifEverySetEmpty NoNutritionAssessment


sortTuplesByDateDesc : ( NominalDate, a ) -> ( NominalDate, a ) -> Order
sortTuplesByDateDesc m1 m2 =
    Date.compare (Tuple.first m2) (Tuple.first m1)
