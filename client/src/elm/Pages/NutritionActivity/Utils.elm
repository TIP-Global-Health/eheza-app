module Pages.NutritionActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , HeightInCm(..)
        , MuacInCm(..)
        , MuacIndication(..)
        , NutritionMeasurement
        , NutritionMeasurements
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, or, unwrap)
import Pages.AcuteIllnessActivity.Utils exposing (healthEducationFormWithDefault, sendToHCFormWithDefault)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData, NutritionAssesment(..))
import Pages.Utils exposing (ifEverySetEmpty, taskCompleted, valueConsideringIsDirtyField)
import RemoteData exposing (RemoteData(..))
import Utils.NominalDate exposing (diffDays)
import ZScore.Model exposing (Kilograms(..))
import ZScore.Utils exposing (zScoreWeightForAge)


generateNutritionAssesment : NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> List NutritionAssesment
generateNutritionAssesment currentDate zscores db assembled =
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
                        >> weightValueFunc
                    )

        weightForAgeZScore =
            calculateZScoreWeightForAge currentDate zscores child weightValue

        weightValueFunc =
            \(WeightInKg kg) -> kg

        groupWeightMeasurements =
            Dict.get assembled.participant.person db.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.weights
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, weightValueFunc measurement.value ))
                    )
                |> Maybe.withDefault []

        previousIndividualWeightMeasurements =
            resolvePreviousIndividualValues assembled .weight weightValueFunc

        individualWeightMeasurements =
            weightValue
                |> Maybe.map (\value -> ( currentDate, value ) :: previousIndividualWeightMeasurements)
                |> Maybe.withDefault previousIndividualWeightMeasurements

        allWeigthMeasuements =
            groupWeightMeasurements
                ++ individualWeightMeasurements
                -- Most recent date to least recent date.
                |> List.sortWith (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))

        assesmentByWeightForAgeZScore =
            weightForAgeZScore
                |> Maybe.andThen
                    (\zScore ->
                        let
                            previousZScore =
                                List.Extra.getAt 1 allWeigthMeasuements
                                    |> Maybe.andThen
                                        (\( date, previousWeightValue ) ->
                                            calculateZScoreWeightForAge date zscores child (Just previousWeightValue)
                                        )
                        in
                        if zScoreWeightForAgeSevere zScore then
                            Just AssesmentUnderweightSevere

                        else if zScoreWeightForAgeModerate currentDate child zScore previousZScore then
                            Just AssesmentUnderweightModerate

                        else
                            Nothing
                    )

        -- 3 consecutive weight losses of minimum 0.5kg per visit
        assesmentByConsecutiveWeight =
            Maybe.andThen
                (\age ->
                    if age < 6 then
                        Nothing

                    else
                        let
                            fourLatest =
                                List.take 4 allWeigthMeasuements
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
            ageInMonths currentDate child

        dangerSigns =
            measurements.nutrition
                |> Maybe.map (Tuple.second >> .value >> EverySet.toList >> List.filter ((/=) NormalChildNutrition))
                |> Maybe.withDefault []

        dangerSignsPresent =
            not <| List.isEmpty dangerSigns
    in
    [ assesmentByMuac, assesmentByWeightForAgeZScore, assesmentByConsecutiveWeight, assementByNutritionSigns ]
        |> List.filterMap identity


expectActivity : NominalDate -> ZScore.Model.Model -> Person -> Bool -> AssembledData -> ModelIndexedDb -> NutritionActivity -> Bool
expectActivity currentDate zscores child isChw data db activity =
    case activity of
        -- Do not show for community health workers.
        Height ->
            not isChw

        -- Show for children that are at least 6 month old.
        Muac ->
            ageInMonths currentDate child
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        NextSteps ->
            if isChw && mandatoryActivitiesCompleted currentDate zscores child isChw data db then
                -- Any assesment require sending to HC.
                generateNutritionAssesment currentDate zscores db data
                    |> List.isEmpty
                    |> not

            else
                False

        _ ->
            True


activityCompleted : NominalDate -> ZScore.Model.Model -> Person -> Bool -> AssembledData -> ModelIndexedDb -> NutritionActivity -> Bool
activityCompleted currentDate zscores child isChw data db activity =
    let
        measurements =
            data.measurements
    in
    case activity of
        Height ->
            (not <| expectActivity currentDate zscores child isChw data db Height)
                || isJust measurements.height

        Muac ->
            (not <| expectActivity currentDate zscores child isChw data db Muac)
                || isJust measurements.muac

        Nutrition ->
            isJust measurements.nutrition

        Photo ->
            isJust measurements.photo

        Weight ->
            isJust measurements.weight

        NextSteps ->
            (not <| expectActivity currentDate zscores child isChw data db NextSteps)
                || (isJust measurements.sendToHC && isJust measurements.healthEducation)


mandatoryActivitiesCompleted : NominalDate -> ZScore.Model.Model -> Person -> Bool -> AssembledData -> ModelIndexedDb -> Bool
mandatoryActivitiesCompleted currentDate zscores child isChw data db =
    [ Height, Muac, Nutrition, Weight ]
        |> List.filter (not << activityCompleted currentDate zscores child isChw data db)
        |> List.isEmpty


nextStepsTasksCompletedFromTotal : NutritionMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal measurements data task =
    case task of
        NextStepsSendToHC ->
            let
                form =
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm

                ( reasonForNotSentActive, reasonForNotSentCompleted ) =
                    form.referToHealthCenter
                        |> Maybe.map
                            (\sentToHC ->
                                if not sentToHC then
                                    if isJust form.reasonForNotSendingToHC then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( reasonForNotSentActive + taskCompleted form.handReferralForm
            , reasonForNotSentCompleted + 1
            )

        NextStepsHealthEducation ->
            let
                form =
                    measurements.healthEducation
                        |> Maybe.map (Tuple.second >> .value)
                        |> healthEducationFormWithDefault data.healthEducationForm

                ( reasonForProvidingEducationActive, reasonForProvidingEducationCompleted ) =
                    form.educationForDiagnosis
                        |> Maybe.map
                            (\providedHealthEducation ->
                                if not providedHealthEducation then
                                    if isJust form.reasonForNotProvidingHealthEducation then
                                        ( 1, 1 )

                                    else
                                        ( 0, 1 )

                                else
                                    ( 0, 0 )
                            )
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( reasonForProvidingEducationActive + taskCompleted form.educationForDiagnosis
            , reasonForProvidingEducationCompleted + 1
            )


resolvePreviousIndividualValues : AssembledData -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a )) -> (a -> b) -> List ( NominalDate, b )
resolvePreviousIndividualValues assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( date, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.map
                        (\measurement ->
                            ( date, Tuple.second measurement |> .value |> valueFunc )
                        )
            )
        |> List.reverse


resolvePreviousIndividualValue : AssembledData -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a )) -> (a -> b) -> Maybe ( NominalDate, b )
resolvePreviousIndividualValue assembled measurementFunc valueFunc =
    resolvePreviousIndividualValues assembled measurementFunc valueFunc
        |> List.head


fromMuacValue : Maybe MuacInCm -> MuacForm
fromMuacValue saved =
    { muac = Maybe.map (\(MuacInCm cm) -> cm) saved
    , muacDirty = False
    }


muacFormWithDefault : MuacForm -> Maybe MuacInCm -> MuacForm
muacFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { muac = valueConsideringIsDirtyField form.muacDirty form.muac (value |> (\(MuacInCm cm) -> cm))
                , muacDirty = form.muacDirty
                }
            )


toMuacValueWithDefault : Maybe MuacInCm -> MuacForm -> Maybe MuacInCm
toMuacValueWithDefault saved form =
    muacFormWithDefault form saved
        |> toMuacValue


toMuacValue : MuacForm -> Maybe MuacInCm
toMuacValue form =
    Maybe.map MuacInCm form.muac


fromHeightValue : Maybe HeightInCm -> HeightForm
fromHeightValue saved =
    { height = Maybe.map (\(HeightInCm cm) -> cm) saved
    , heightDirty = False
    }


heightFormWithDefault : HeightForm -> Maybe HeightInCm -> HeightForm
heightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (value |> (\(HeightInCm cm) -> cm))
                , heightDirty = form.heightDirty
                }
            )


toHeightValueWithDefault : Maybe HeightInCm -> HeightForm -> Maybe HeightInCm
toHeightValueWithDefault saved form =
    heightFormWithDefault form saved
        |> toHeightValue


toHeightValue : HeightForm -> Maybe HeightInCm
toHeightValue form =
    Maybe.map HeightInCm form.height


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : NutritionForm -> Maybe (EverySet ChildNutritionSign) -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs


fromWeightValue : Maybe WeightInKg -> WeightForm
fromWeightValue saved =
    { weight = Maybe.map (\(WeightInKg cm) -> cm) saved
    , weightDirty = False
    }


weightFormWithDefault : WeightForm -> Maybe WeightInKg -> WeightForm
weightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { weight = valueConsideringIsDirtyField form.weightDirty form.weight (value |> (\(WeightInKg cm) -> cm))
                , weightDirty = form.weightDirty
                }
            )


toWeightValueWithDefault : Maybe WeightInKg -> WeightForm -> Maybe WeightInKg
toWeightValueWithDefault saved form =
    weightFormWithDefault form saved
        |> toWeightValue


toWeightValue : WeightForm -> Maybe WeightInKg
toWeightValue form =
    Maybe.map WeightInKg form.weight


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


zScoreWeightForAgeSevere : Float -> Bool
zScoreWeightForAgeSevere zScore =
    zScore <= -3


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


muacSevere : MuacInCm -> Bool
muacSevere muac =
    muacIndication muac == MuacRed


muacModerate : MuacInCm -> Bool
muacModerate muac =
    muacIndication muac == MuacYellow
