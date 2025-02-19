module Pages.WellChild.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( expectNCDAActivity
        , getMeasurementValueFunc
        , headCircumferenceIndication
        , headCircumferenceValueFunc
        , weightValueFunc
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( contributingFactorsFormInutsAndTasks
        , followUpFormInputsAndTasks
        , healthEducationFormInutsAndTasks
        , heightFormAndTasks
        , nutritionFormInputsAndTasks
        , referToProgramFormInputsAndTasks
        , sendToFacilityInputsAndTasks
        , viewColorAlertIndication
        , vitalsFormInputsAndTasks
        , weightFormAndTasks
        )
import Pages.AcuteIllness.Activity.Utils exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationQuestion)
import Pages.Utils
    exposing
        ( concatInputsAndTasksSections
        , ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , resolveTasksCompletedFromTotal
        , taskAnyCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewPreviousMeasurement
        , viewQuestionLabel
        )
import Pages.WellChild.Activity.Model exposing (..)
import Pages.WellChild.Activity.Types exposing (..)
import Pages.WellChild.Encounter.Model exposing (AssembledData)
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (TranslationId, translate)
import Translate.Model exposing (Language(..))
import ZScore.Model
import ZScore.Utils exposing (viewZScore)


generateNutritionAssessment : NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> List NutritionAssessment
generateNutritionAssessment currentDate zscores db assembled =
    let
        measurements =
            assembled.measurements

        muacValue =
            getMeasurementValueFunc measurements.muac

        nutritionValue =
            getMeasurementValueFunc measurements.nutrition
                |> Maybe.map .signs

        weightValue =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> weightValueFunc
                )
                measurements.weight
    in
    Backend.NutritionEncounter.Utils.generateNutritionAssessment currentDate zscores assembled.participant.person muacValue nutritionValue weightValue False db


activityCompleted :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> WellChildActivity
    -> Bool
activityCompleted currentDate zscores site features isChw assembled db activity =
    let
        measurements =
            assembled.measurements

        activityExpected =
            expectActivity currentDate zscores site features isChw assembled db
    in
    case activity of
        WellChildPregnancySummary ->
            (not <| activityExpected WellChildPregnancySummary)
                || isJust measurements.pregnancySummary

        WellChildDangerSigns ->
            (not <| activityExpected WellChildDangerSigns)
                || (isJust measurements.symptomsReview && isJust measurements.vitals)

        WellChildNutritionAssessment ->
            resolveNutritionAssessmentTasks assembled
                |> List.all (nutritionAssessmentTaskCompleted currentDate assembled)

        WellChildECD ->
            (not <| activityExpected WellChildECD) || isJust measurements.ecd

        WellChildMedication ->
            (not <| activityExpected WellChildMedication)
                || List.all (medicationTaskCompleted currentDate site isChw assembled) medicationTasks

        WellChildImmunisation ->
            (not <| activityExpected WellChildImmunisation)
                || List.all (immunisationTaskCompleted currentDate site isChw assembled db) immunisationVaccinationTasks

        WellChildNextSteps ->
            List.all (nextStepsTaskCompleted currentDate zscores site features isChw assembled db) nextStepsTasks

        WellChildPhoto ->
            (not <| activityExpected WellChildPhoto) || isJust measurements.photo

        WellChildNCDA ->
            (not <| activityExpected WellChildNCDA) || isJust measurements.ncda

        WellChildHomeVisit ->
            (not <| activityExpected WellChildHomeVisit)
                || (isJust measurements.caring
                        && isJust measurements.feeding
                        && isJust measurements.hygiene
                        && isJust measurements.foodSecurity
                   )


expectActivity :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> WellChildActivity
    -> Bool
expectActivity currentDate zscores site features isChw assembled db activity =
    case activity of
        WellChildPregnancySummary ->
            assembled.encounter.encounterType == NewbornExam

        WellChildDangerSigns ->
            assembled.encounter.encounterType /= NewbornExam

        WellChildNutritionAssessment ->
            True

        WellChildImmunisation ->
            behindOnVaccinationsByHistory currentDate
                site
                assembled.person
                assembled.vaccinationHistory
                assembled.vaccinationProgress

        WellChildECD ->
            (assembled.encounter.encounterType == PediatricCare)
                && (generateRemianingECDSignsBeforeCurrentEncounter currentDate assembled
                        |> List.isEmpty
                        |> not
                   )

        WellChildMedication ->
            (assembled.encounter.encounterType == PediatricCare)
                && (List.filter (expectMedicationTask currentDate site isChw assembled) medicationTasks
                        |> List.isEmpty
                        |> not
                   )

        WellChildNextSteps ->
            List.filter (expectNextStepsTask currentDate zscores site features isChw assembled db) nextStepsTasks
                |> List.isEmpty
                |> not

        WellChildPhoto ->
            True

        WellChildNCDA ->
            -- For nurses only, show if child is bellow age of 24 months.
            (assembled.encounter.encounterType == PediatricCare)
                && expectNCDAActivity currentDate features isChw assembled.person

        WellChildHomeVisit ->
            assembled.encounter.encounterType == PediatricCareChw


generateVaccinationProgress : Site -> Person -> List WellChildMeasurements -> VaccinationProgressDict
generateVaccinationProgress =
    Measurement.Utils.generateVaccinationProgressForWellChild


fromPregnancySummaryValue : Maybe PregnancySummaryValue -> PregnancySummaryForm
fromPregnancySummaryValue saved =
    let
        deliveryComplications =
            Maybe.map (.deliveryComplications >> EverySet.toList) saved

        deliveryComplicationsPresent =
            Maybe.map (listNotEmptyWithException NoDeliveryComplications) deliveryComplications

        birthDefects =
            Maybe.map (.birthDefects >> EverySet.toList) saved

        birthDefectsPresent =
            Maybe.map (listNotEmptyWithException NoBirthDefects) birthDefects

        signs =
            Maybe.map (.signs >> EverySet.toList) saved
    in
    { expectedDateConcluded = Maybe.map .expectedDateConcluded saved
    , dateSelectorPopupState = Nothing
    , deliveryComplicationsPresent = deliveryComplicationsPresent
    , deliveryComplications = deliveryComplications
    , apgarScoresAvailable = Maybe.map (List.member ApgarScores) signs
    , apgarOneMin = Maybe.andThen .apgarOneMin saved
    , apgarFiveMin = Maybe.andThen .apgarFiveMin saved
    , apgarDirty = False
    , birthWeight = Maybe.andThen .birthWeight saved
    , birthLengthAvailable = Maybe.map (List.member BirthLength) signs
    , birthLength = Maybe.andThen .birthLength saved
    , birthLengthDirty = False
    , birthDefectsPresent = birthDefectsPresent
    , birthDefects = birthDefects
    }


pregnancySummaryFormWithDefault : PregnancySummaryForm -> Maybe PregnancySummaryValue -> PregnancySummaryForm
pregnancySummaryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    deliveryComplications =
                        if form.deliveryComplicationsPresent == Just False then
                            [ NoDeliveryComplications ]

                        else
                            EverySet.toList value.deliveryComplications

                    birthDefects =
                        if form.birthDefectsPresent == Just False then
                            [ NoBirthDefects ]

                        else
                            EverySet.toList value.birthDefects

                    signsFromValue =
                        EverySet.toList value.signs
                in
                { expectedDateConcluded = or form.expectedDateConcluded (Just value.expectedDateConcluded)
                , dateSelectorPopupState = form.dateSelectorPopupState
                , deliveryComplicationsPresent =
                    or form.deliveryComplicationsPresent
                        (listNotEmptyWithException NoDeliveryComplications deliveryComplications |> Just)
                , deliveryComplications = or form.deliveryComplications (Just deliveryComplications)
                , apgarScoresAvailable = or form.apgarScoresAvailable (List.member ApgarScores signsFromValue |> Just)
                , apgarOneMin = or form.apgarOneMin (Maybe.andThen .apgarOneMin saved)
                , apgarFiveMin = or form.apgarFiveMin (Maybe.andThen .apgarFiveMin saved)
                , apgarDirty = form.apgarDirty
                , birthWeight = or form.birthWeight (Maybe.andThen .birthWeight saved)
                , birthLengthAvailable = or form.birthLengthAvailable (List.member BirthLength signsFromValue |> Just)
                , birthLength = or form.birthLength (Maybe.andThen .birthLength saved)
                , birthLengthDirty = form.birthLengthDirty
                , birthDefectsPresent =
                    or form.birthDefectsPresent
                        (listNotEmptyWithException NoBirthDefects birthDefects |> Just)
                , birthDefects = or form.birthDefects (Just birthDefects)
                }
            )


toPregnancySummaryValueWithDefault : Maybe PregnancySummaryValue -> PregnancySummaryForm -> Maybe PregnancySummaryValue
toPregnancySummaryValueWithDefault saved form =
    pregnancySummaryFormWithDefault form saved
        |> toPregnancySummaryValue


toPregnancySummaryValue : PregnancySummaryForm -> Maybe PregnancySummaryValue
toPregnancySummaryValue form =
    let
        deliveryComplications =
            Maybe.map EverySet.fromList form.deliveryComplications
                |> Maybe.withDefault (EverySet.singleton NoDeliveryComplications)

        signs =
            [ ifNullableTrue ApgarScores form.apgarScoresAvailable
            , ifNullableTrue BirthLength form.birthLengthAvailable
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPregnancySummarySigns)

        birthDefects =
            Maybe.map EverySet.fromList form.birthDefects
                |> Maybe.withDefault (EverySet.singleton NoBirthDefects)
    in
    Maybe.map PregnancySummaryValue form.expectedDateConcluded
        |> andMap (Just deliveryComplications)
        |> andMap signs
        |> andMap (Just form.apgarOneMin)
        |> andMap (Just form.apgarFiveMin)
        |> andMap (Just form.birthWeight)
        |> andMap (Just form.birthLength)
        |> andMap (Just birthDefects)


listNotEmptyWithException : a -> List a -> Bool
listNotEmptyWithException exception list =
    if List.isEmpty list then
        False

    else
        list /= [ exception ]


nutritionAssessmentTaskCompleted : NominalDate -> AssembledData -> NutritionAssessmentTask -> Bool
nutritionAssessmentTaskCompleted currentDate assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectNutritionAssessmentTask currentDate assembled
    in
    case task of
        TaskHeight ->
            (not <| taskExpected TaskHeight) || isJust measurements.height

        TaskHeadCircumference ->
            (not <| taskExpected TaskHeadCircumference) || isJust measurements.headCircumference

        TaskMuac ->
            (not <| taskExpected TaskMuac) || isJust measurements.muac

        TaskNutrition ->
            (not <| taskExpected TaskNutrition) || isJust measurements.nutrition

        TaskWeight ->
            (not <| taskExpected TaskWeight) || isJust measurements.weight


expectNutritionAssessmentTask : NominalDate -> AssembledData -> NutritionAssessmentTask -> Bool
expectNutritionAssessmentTask currentDate assembled task =
    case task of
        -- Show for children that are up to 3 years old.
        TaskHeadCircumference ->
            ageInMonths currentDate assembled.person
                |> Maybe.map (\ageMonths -> ageMonths < 36)
                |> Maybe.withDefault False

        -- Show for children that are at least 6 month old.
        TaskMuac ->
            ageInMonths currentDate assembled.person
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        -- View any other task.
        _ ->
            True


mandatoryNutritionAssessmentTasksCompleted : NominalDate -> AssembledData -> Bool
mandatoryNutritionAssessmentTasksCompleted currentDate assembled =
    resolveMandatoryNutritionAssessmentTasks currentDate assembled
        |> List.filter (not << nutritionAssessmentTaskCompleted currentDate assembled)
        |> List.isEmpty


resolveMandatoryNutritionAssessmentTasks : NominalDate -> AssembledData -> List NutritionAssessmentTask
resolveMandatoryNutritionAssessmentTasks currentDate assembled =
    List.filter (expectNutritionAssessmentTask currentDate assembled) <|
        case assembled.encounter.encounterType of
            PediatricCare ->
                [ TaskHeight, TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ]

            _ ->
                -- Height is optional for CHW.
                [ TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ]


resolveNutritionAssessmentTasks : AssembledData -> List NutritionAssessmentTask
resolveNutritionAssessmentTasks assembled =
    case assembled.encounter.encounterType of
        NewbornExam ->
            -- Height and Muac are not here, because Newbor Exam
            -- is done for children that are less than 2 months old.
            [ TaskHeadCircumference, TaskNutrition, TaskWeight ]

        _ ->
            [ TaskHeight, TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ]


nutritionAssessmentTasksCompletedFromTotal :
    NominalDate
    -> ZScore.Model.Model
    -> AssembledData
    -> NutritionAssessmentData
    -> NutritionAssessmentTask
    -> ( Int, Int )
nutritionAssessmentTasksCompletedFromTotal currentDate zscores assembled data task =
    let
        measurements =
            assembled.measurements

        ( _, tasks ) =
            case task of
                TaskHeight ->
                    getMeasurementValueFunc measurements.height
                        |> heightFormWithDefault data.heightForm
                        |> heightFormAndTasks English
                            currentDate
                            zscores
                            assembled.person
                            Nothing
                            SetHeight

                TaskHeadCircumference ->
                    getMeasurementValueFunc measurements.headCircumference
                        |> headCircumferenceFormWithDefault data.headCircumferenceForm
                        |> headCircumferenceFormAndTasks English
                            currentDate
                            assembled.person
                            Nothing
                            Nothing

                TaskMuac ->
                    getMeasurementValueFunc measurements.muac
                        |> muacFormWithDefault data.muacForm
                        |> Measurement.View.muacFormInputsAndTasks English
                            currentDate
                            SiteRwanda
                            assembled.person
                            Nothing
                            Pages.WellChild.Activity.Model.SetMuac

                TaskNutrition ->
                    getMeasurementValueFunc measurements.nutrition
                        |> nutritionFormWithDefault data.nutritionForm
                        |> nutritionFormInputsAndTasks English
                            currentDate
                            Pages.WellChild.Activity.Model.SetNutritionSign

                TaskWeight ->
                    getMeasurementValueFunc measurements.weight
                        |> weightFormWithDefault data.weightForm
                        |> weightFormAndTasks English
                            currentDate
                            zscores
                            assembled.person
                            Nothing
                            Nothing
                            False
                            Pages.WellChild.Activity.Model.SetWeight
    in
    resolveTasksCompletedFromTotal tasks


headCircumferenceFormAndTasks :
    Language
    -> NominalDate
    -> Person
    -> Maybe Float
    -> Maybe Float
    -> HeadCircumferenceForm
    -> ( List (Html Msg), List (Maybe Bool) )
headCircumferenceFormAndTasks language currentDate person zscore previousValue form =
    let
        inputSection =
            if measurementNotTakenChecked then
                []

            else
                let
                    zScoreText =
                        Maybe.map viewZScore zscore
                            |> Maybe.withDefault (translate language Translate.NotAvailable)
                in
                [ div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewMeasurementInput
                            language
                            form.headCircumference
                            SetHeadCircumference
                            "head-circumference"
                            Translate.UnitCentimeter
                        ]
                    , div
                        [ class "five wide column" ]
                        [ showMaybe <|
                            Maybe.map (HeadCircumferenceInCm >> headCircumferenceIndication >> viewColorAlertIndication language) zscore
                        ]
                    ]
                , viewPreviousMeasurement language previousValue Translate.UnitCentimeter
                , div [ class "ui large header z-score age" ]
                    [ text <| translate language Translate.ZScoreHeadCircumferenceForAge
                    , span [ class "sub header" ]
                        [ text zScoreText ]
                    ]
                ]

        measurementNotTakenChecked =
            form.measurementNotTaken == Just True
    in
    ( [ div [ class "ui form head-circumference" ] <|
            [ viewLabel language <| Translate.NutritionAssessmentTask TaskHeadCircumference
            , p [ class "activity-helper" ] [ text <| translate language Translate.HeadCircumferenceHelper ]
            ]
                ++ inputSection
      , div
            [ class "ui checkbox activity"
            , onClick ToggleHeadCircumferenceNotTaken
            ]
            [ input
                [ type_ "checkbox"
                , checked measurementNotTakenChecked
                , classList [ ( "checked", measurementNotTakenChecked ) ]
                ]
                []
            , label [] [ text <| translate language Translate.HeadCircumferenceNotTakenLabel ]
            ]
      ]
    , [ maybeToBoolTask form.headCircumference ]
    )


fromSymptomsReviewValue : Maybe (EverySet WellChildSymptom) -> SymptomsReviewForm
fromSymptomsReviewValue saved =
    { symptoms = Maybe.map EverySet.toList saved }


symptomsReviewFormWithDefault : SymptomsReviewForm -> Maybe (EverySet WellChildSymptom) -> SymptomsReviewForm
symptomsReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { symptoms = or form.symptoms (EverySet.toList value |> Just) }
            )


toSymptomsReviewValueWithDefault : Maybe (EverySet WellChildSymptom) -> SymptomsReviewForm -> Maybe (EverySet WellChildSymptom)
toSymptomsReviewValueWithDefault saved form =
    symptomsReviewFormWithDefault form saved
        |> toSymptomsReviewValue


toSymptomsReviewValue : SymptomsReviewForm -> Maybe (EverySet WellChildSymptom)
toSymptomsReviewValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoWellChildSymptoms) form.symptoms


wellChildECDFormWithDefault : WellChildECDForm -> Maybe (EverySet ECDSign) -> WellChildECDForm
wellChildECDFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\signs ->
                { followMothersEyes = or form.followMothersEyes (EverySet.member FollowMothersEyes signs |> Just)
                , moveArmsAndLegs = or form.moveArmsAndLegs (EverySet.member MoveArmsAndLegs signs |> Just)
                , raiseHandsUp = or form.raiseHandsUp (EverySet.member RaiseHandsUp signs |> Just)
                , smile = or form.smile (EverySet.member Smile signs |> Just)
                , rollSideways = or form.rollSideways (EverySet.member RollSideways signs |> Just)
                , bringHandsToMouth = or form.bringHandsToMouth (EverySet.member BringHandsToMouth signs |> Just)
                , holdHeadWithoutSupport = or form.holdHeadWithoutSupport (EverySet.member HoldHeadWithoutSupport signs |> Just)
                , holdAndShakeToys = or form.holdAndShakeToys (EverySet.member HoldAndShakeToys signs |> Just)
                , reactToSuddenSounds = or form.reactToSuddenSounds (EverySet.member ReactToSuddenSounds signs |> Just)
                , useConsonantSounds = or form.useConsonantSounds (EverySet.member UseConsonantSounds signs |> Just)
                , respondToSoundWithSound = or form.respondToSoundWithSound (EverySet.member RespondToSoundWithSound signs |> Just)
                , turnHeadWhenCalled = or form.turnHeadWhenCalled (EverySet.member TurnHeadWhenCalled signs |> Just)
                , sitWithoutSupport = or form.sitWithoutSupport (EverySet.member SitWithoutSupport signs |> Just)
                , smileBack = or form.smileBack (EverySet.member SmileBack signs |> Just)
                , rollTummyToBack = or form.rollTummyToBack (EverySet.member RollTummyToBack signs |> Just)
                , reachForToys = or form.reachForToys (EverySet.member ReachForToys signs |> Just)
                , useSimpleGestures = or form.useSimpleGestures (EverySet.member UseSimpleGestures signs |> Just)
                , standOnTheirOwn = or form.standOnTheirOwn (EverySet.member StandOnTheirOwn signs |> Just)
                , copyDuringPlay = or form.copyDuringPlay (EverySet.member CopyDuringPlay signs |> Just)
                , sayMamaDada = or form.sayMamaDada (EverySet.member SayMamaDada signs |> Just)
                , canHoldSmallObjects = or form.canHoldSmallObjects (EverySet.member CanHoldSmallObjects signs |> Just)
                , looksWhenPointedAt = or form.looksWhenPointedAt (EverySet.member LooksWhenPointedAt signs |> Just)
                , useSingleWords = or form.useSingleWords (EverySet.member UseSingleWords signs |> Just)
                , walkWithoutHelp = or form.walkWithoutHelp (EverySet.member WalkWithoutHelp signs |> Just)
                , playPretend = or form.playPretend (EverySet.member PlayPretend signs |> Just)
                , pointToThingsOfInterest = or form.pointToThingsOfInterest (EverySet.member PointToThingsOfInterest signs |> Just)
                , useShortPhrases = or form.useShortPhrases (EverySet.member UseShortPhrases signs |> Just)
                , interestedInOtherChildren = or form.interestedInOtherChildren (EverySet.member InterestedInOtherChildren signs |> Just)
                , followSimlpeInstructions = or form.followSimlpeInstructions (EverySet.member FollowSimpleInstructions signs |> Just)
                , kickBall = or form.kickBall (EverySet.member KickBall signs |> Just)
                , pointAtNamedObjects = or form.pointAtNamedObjects (EverySet.member PointAtNamedObjects signs |> Just)
                , dressThemselves = or form.dressThemselves (EverySet.member DressThemselves signs |> Just)
                , washHandsGoToToiled = or form.washHandsGoToToiled (EverySet.member WashHandsGoToToiled signs |> Just)
                , knowsColorsAndNumbers = or form.knowsColorsAndNumbers (EverySet.member KnowsColorsAndNumbers signs |> Just)
                , useMediumPhrases = or form.useMediumPhrases (EverySet.member UseMediumPhrases signs |> Just)
                , playMakeBelieve = or form.playMakeBelieve (EverySet.member PlayMakeBelieve signs |> Just)
                , followThreeStepInstructions = or form.followThreeStepInstructions (EverySet.member FollowThreeStepInstructions signs |> Just)
                , standOnOneFootFiveSeconds = or form.standOnOneFootFiveSeconds (EverySet.member StandOnOneFootFiveSeconds signs |> Just)
                , useLongPhrases = or form.useLongPhrases (EverySet.member UseLongPhrases signs |> Just)
                , shareWithOtherChildren = or form.shareWithOtherChildren (EverySet.member ShareWithOtherChildren signs |> Just)
                , countToTen = or form.countToTen (EverySet.member CountToTen signs |> Just)
                }
            )


toWellChildECDValueWithDefault : Maybe (EverySet ECDSign) -> WellChildECDForm -> Maybe (EverySet ECDSign)
toWellChildECDValueWithDefault saved form =
    wellChildECDFormWithDefault form saved
        |> toWellChildECDValue


toWellChildECDValue : WellChildECDForm -> Maybe (EverySet ECDSign)
toWellChildECDValue form =
    [ ifNullableTrue FollowMothersEyes form.followMothersEyes
    , ifNullableTrue MoveArmsAndLegs form.moveArmsAndLegs
    , ifNullableTrue RaiseHandsUp form.raiseHandsUp
    , ifNullableTrue Smile form.smile
    , ifNullableTrue RollSideways form.rollSideways
    , ifNullableTrue BringHandsToMouth form.bringHandsToMouth
    , ifNullableTrue HoldHeadWithoutSupport form.holdHeadWithoutSupport
    , ifNullableTrue HoldAndShakeToys form.holdAndShakeToys
    , ifNullableTrue ReactToSuddenSounds form.reactToSuddenSounds
    , ifNullableTrue UseConsonantSounds form.useConsonantSounds
    , ifNullableTrue RespondToSoundWithSound form.respondToSoundWithSound
    , ifNullableTrue TurnHeadWhenCalled form.turnHeadWhenCalled
    , ifNullableTrue SitWithoutSupport form.sitWithoutSupport
    , ifNullableTrue SmileBack form.smileBack
    , ifNullableTrue RollTummyToBack form.rollTummyToBack
    , ifNullableTrue ReachForToys form.reachForToys
    , ifNullableTrue UseSimpleGestures form.useSimpleGestures
    , ifNullableTrue StandOnTheirOwn form.standOnTheirOwn
    , ifNullableTrue CopyDuringPlay form.copyDuringPlay
    , ifNullableTrue SayMamaDada form.sayMamaDada
    , ifNullableTrue CanHoldSmallObjects form.canHoldSmallObjects
    , ifNullableTrue LooksWhenPointedAt form.looksWhenPointedAt
    , ifNullableTrue UseSingleWords form.useSingleWords
    , ifNullableTrue WalkWithoutHelp form.walkWithoutHelp
    , ifNullableTrue PlayPretend form.playPretend
    , ifNullableTrue PointToThingsOfInterest form.pointToThingsOfInterest
    , ifNullableTrue UseShortPhrases form.useShortPhrases
    , ifNullableTrue InterestedInOtherChildren form.interestedInOtherChildren
    , ifNullableTrue FollowSimpleInstructions form.followSimlpeInstructions
    , ifNullableTrue KickBall form.kickBall
    , ifNullableTrue PointAtNamedObjects form.pointAtNamedObjects
    , ifNullableTrue DressThemselves form.dressThemselves
    , ifNullableTrue WashHandsGoToToiled form.washHandsGoToToiled
    , ifNullableTrue KnowsColorsAndNumbers form.knowsColorsAndNumbers
    , ifNullableTrue UseMediumPhrases form.useMediumPhrases
    , ifNullableTrue PlayMakeBelieve form.playMakeBelieve
    , ifNullableTrue FollowThreeStepInstructions form.followThreeStepInstructions
    , ifNullableTrue StandOnOneFootFiveSeconds form.standOnOneFootFiveSeconds
    , ifNullableTrue UseLongPhrases form.useLongPhrases
    , ifNullableTrue ShareWithOtherChildren form.shareWithOtherChildren
    , ifNullableTrue CountToTen form.countToTen
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoECDSigns)


fromHeadCircumferenceValue : Maybe HeadCircumferenceValue -> HeadCircumferenceForm
fromHeadCircumferenceValue saved =
    { headCircumference = Maybe.map (.headCircumference >> headCircumferenceValueFunc) saved
    , headCircumferenceDirty = False
    , measurementNotTaken = Maybe.andThen (.notes >> EverySet.member NoteNotTaken >> Just) saved
    }


headCircumferenceFormWithDefault : HeadCircumferenceForm -> Maybe HeadCircumferenceValue -> HeadCircumferenceForm
headCircumferenceFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { headCircumference = valueConsideringIsDirtyField form.headCircumferenceDirty form.headCircumference (headCircumferenceValueFunc value.headCircumference)
                , headCircumferenceDirty = form.headCircumferenceDirty
                , measurementNotTaken = or form.measurementNotTaken (EverySet.member NoteNotTaken value.notes |> Just)
                }
            )


toHeadCircumferenceValueWithDefault : Maybe HeadCircumferenceValue -> HeadCircumferenceForm -> Maybe HeadCircumferenceValue
toHeadCircumferenceValueWithDefault saved form =
    headCircumferenceFormWithDefault form saved
        |> toHeadCircumferenceValue


toHeadCircumferenceValue : HeadCircumferenceForm -> Maybe HeadCircumferenceValue
toHeadCircumferenceValue form =
    let
        headCircumference =
            Maybe.map (\cm -> HeadCircumferenceInCm cm) form.headCircumference

        notes =
            [ Maybe.map (ifTrue NoteNotTaken) form.measurementNotTaken ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMeasurementNotes)
    in
    Maybe.map HeadCircumferenceValue headCircumference
        |> andMap notes


dangerSignsTasksCompletedFromTotal : NominalDate -> AssembledData -> DangerSignsData -> DangerSignsTask -> ( Int, Int )
dangerSignsTasksCompletedFromTotal currentDate assembled data task =
    let
        measurements =
            assembled.measurements

        ( _, tasks ) =
            case task of
                TaskSymptomsReview ->
                    getMeasurementValueFunc measurements.symptomsReview
                        |> symptomsReviewFormWithDefault data.symptomsReviewForm
                        |> symptomsReviewFormInputsAndTasks English currentDate

                TaskVitals ->
                    let
                        formConfig =
                            generateVitalsFormConfig assembled
                    in
                    getMeasurementValueFunc measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> vitalsFormInputsAndTasks English currentDate formConfig
    in
    resolveTasksCompletedFromTotal tasks


symptomsReviewFormInputsAndTasks : Language -> NominalDate -> SymptomsReviewForm -> ( List (Html Msg), List (Maybe Bool) )
symptomsReviewFormInputsAndTasks language currentDate form =
    ( [ viewQuestionLabel language Translate.PatientGotAnySymptoms
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ SymptomBreathingProblems
            , SymptomConvulsions
            , SymptomLethargyOrUnresponsiveness
            , SymptomDiarrhea
            , SymptomVomiting
            , SymptomUmbilicalCordRedness
            , SymptomStiffNeckOrBulgingFontanelle
            , SymptomSevereEdema
            , SymptomPalmoplantarPallor
            , SymptomHistoryOfFever
            , SymptomBabyTiresQuicklyWhenFeeding
            , SymptomCoughingOrTearingWhileFeeding
            , SymptomRigidMusclesOrJawClenchingPreventingFeeding
            , ExcessiveSweatingWhenFeeding
            ]
            []
            (Maybe.withDefault [] form.symptoms)
            (Just NoWellChildSymptoms)
            SetSymptom
            Translate.WellChildSymptom
      ]
    , [ maybeToBoolTask form.symptoms ]
    )


generateVitalsFormConfig : AssembledData -> VitalsFormConfig Msg
generateVitalsFormConfig assembled =
    { setIntInputMsg = SetVitalsIntInput
    , setFloatInputMsg = SetVitalsFloatInput
    , sysBloodPressurePreviousValue = Nothing
    , diaBloodPressurePreviousValue = Nothing
    , heartRatePreviousValue = Nothing
    , respiratoryRatePreviousValue =
        resolvePreviousValue assembled .vitals .respiratoryRate
            |> Maybe.map toFloat
    , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
    , birthDate = assembled.person.birthDate
    , formClass = "vitals"
    , mode = VitalsFormBasic
    , invokationModule = InvokationModuleWellChild
    }


immunisationTaskCompleted : NominalDate -> Site -> Bool -> AssembledData -> ModelIndexedDb -> Measurement.Model.ImmunisationTask -> Bool
immunisationTaskCompleted currentDate site isChw data db task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectImmunisationTask currentDate site isChw data
    in
    case task of
        TaskBCG ->
            (not <| taskExpected TaskBCG) || isJust measurements.bcgImmunisation

        TaskDTP ->
            (not <| taskExpected TaskDTP) || isJust measurements.dtpImmunisation

        TaskDTPStandalone ->
            (not <| taskExpected TaskDTPStandalone) || isJust measurements.dtpStandaloneImmunisation

        TaskHPV ->
            (not <| taskExpected TaskHPV) || isJust measurements.hpvImmunisation

        TaskIPV ->
            (not <| taskExpected TaskIPV) || isJust measurements.ipvImmunisation

        TaskMR ->
            (not <| taskExpected TaskMR) || isJust measurements.mrImmunisation

        TaskOPV ->
            (not <| taskExpected TaskOPV) || isJust measurements.opvImmunisation

        TaskPCV13 ->
            (not <| taskExpected TaskPCV13) || isJust measurements.pcv13Immunisation

        TaskRotarix ->
            (not <| taskExpected TaskRotarix) || isJust measurements.rotarixImmunisation

        TaskOverview ->
            not <| taskExpected TaskOverview


expectImmunisationTask : NominalDate -> Site -> Bool -> AssembledData -> Measurement.Model.ImmunisationTask -> Bool
expectImmunisationTask currentDate site isChw assembled task =
    let
        futureVaccinations =
            generateFutureVaccinationsData currentDate
                site
                assembled.person.birthDate
                assembled.person.gender
                False
                assembled.vaccinationHistory
                |> Dict.fromList

        isTaskExpected vaccineType =
            Dict.get vaccineType futureVaccinations
                |> Maybe.Extra.join
                |> Maybe.map (\( _, date ) -> not <| Date.compare date currentDate == GT)
                |> Maybe.withDefault False
    in
    immunisationTaskToVaccineType task
        |> Maybe.map isTaskExpected
        -- Only task that is not converted to vaccine type
        -- is 'Overview', which we always show.
        |> Maybe.withDefault True


immunisationVaccinationTasks : List ImmunisationTask
immunisationVaccinationTasks =
    [ TaskBCG
    , TaskOPV
    , TaskDTP
    , TaskDTPStandalone
    , TaskPCV13
    , TaskRotarix
    , TaskIPV
    , TaskMR
    , TaskHPV
    ]


immunisationTasks : List ImmunisationTask
immunisationTasks =
    immunisationVaccinationTasks ++ [ TaskOverview ]


getFormByVaccineTypeFunc : WellChildVaccineType -> (ImmunisationData -> WellChildVaccinationForm)
getFormByVaccineTypeFunc vaccineType =
    case vaccineType of
        VaccineBCG ->
            .bcgForm

        VaccineDTP ->
            .dtpForm

        VaccineDTPStandalone ->
            .dtpStandaloneForm

        VaccineHPV ->
            .hpvForm

        VaccineIPV ->
            .ipvForm

        VaccineMR ->
            .mrForm

        VaccineOPV ->
            .opvForm

        VaccinePCV13 ->
            .pcv13Form

        VaccineRotarix ->
            .rotarixForm


updateVaccinationFormByVaccineType : WellChildVaccineType -> WellChildVaccinationForm -> ImmunisationData -> ImmunisationData
updateVaccinationFormByVaccineType vaccineType form data =
    case vaccineType of
        VaccineBCG ->
            { data | bcgForm = form }

        VaccineDTP ->
            { data | dtpForm = form }

        VaccineDTPStandalone ->
            { data | dtpStandaloneForm = form }

        VaccineHPV ->
            { data | hpvForm = form }

        VaccineIPV ->
            { data | ipvForm = form }

        VaccineMR ->
            { data | mrForm = form }

        VaccineOPV ->
            { data | opvForm = form }

        VaccinePCV13 ->
            { data | pcv13Form = form }

        VaccineRotarix ->
            { data | rotarixForm = form }


getMeasurementByVaccineTypeFunc : WellChildVaccineType -> WellChildMeasurements -> Maybe VaccinationValue
getMeasurementByVaccineTypeFunc vaccineType measurements =
    case vaccineType of
        VaccineBCG ->
            measurements.bcgImmunisation
                |> getMeasurementValueFunc

        VaccineDTP ->
            measurements.dtpImmunisation
                |> getMeasurementValueFunc

        VaccineDTPStandalone ->
            measurements.dtpStandaloneImmunisation
                |> getMeasurementValueFunc

        VaccineHPV ->
            measurements.hpvImmunisation
                |> getMeasurementValueFunc

        VaccineIPV ->
            measurements.ipvImmunisation
                |> getMeasurementValueFunc

        VaccineMR ->
            measurements.mrImmunisation
                |> getMeasurementValueFunc

        VaccineOPV ->
            measurements.opvImmunisation
                |> getMeasurementValueFunc

        VaccinePCV13 ->
            measurements.pcv13Immunisation
                |> getMeasurementValueFunc

        VaccineRotarix ->
            measurements.rotarixImmunisation
                |> getMeasurementValueFunc


generateRemianingECDSignsBeforeCurrentEncounter : NominalDate -> AssembledData -> List ECDSign
generateRemianingECDSignsBeforeCurrentEncounter currentDate assembled =
    getPreviousMeasurements assembled.previousMeasurementsWithDates
        |> generateRemianingECDSigns currentDate assembled


generateRemianingECDSignsAfterCurrentEncounter : NominalDate -> AssembledData -> List ECDSign
generateRemianingECDSignsAfterCurrentEncounter currentDate assembled =
    (assembled.measurements :: getPreviousMeasurements assembled.previousMeasurementsWithDates)
        |> generateRemianingECDSigns currentDate assembled


generateRemianingECDSigns : NominalDate -> AssembledData -> List WellChildMeasurements -> List ECDSign
generateRemianingECDSigns currentDate assembled measurementsData =
    let
        completed =
            generateCompletedECDSigns measurementsData
    in
    expectedECDSignsByAge currentDate assembled
        |> List.filter (\sign -> not <| List.member sign completed)


generateCompletedECDSigns : List WellChildMeasurements -> List ECDSign
generateCompletedECDSigns measurementsData =
    measurementsData
        |> List.concatMap
            (\measurements ->
                measurements.ecd
                    |> Maybe.map (Tuple.second >> .value >> EverySet.toList)
                    |> Maybe.withDefault []
            )
        |> List.filter ((/=) NoECDSigns)
        -- Eliminate duplicate occurances.
        |> Pages.Utils.unique


expectedECDSignsByAge : NominalDate -> AssembledData -> List ECDSign
expectedECDSignsByAge currentDate assembled =
    assembled.person.birthDate
        |> Maybe.map
            (\birthDate ->
                let
                    ageWeeks =
                        Date.diff Weeks birthDate currentDate

                    ageMonths =
                        Date.diff Months birthDate currentDate

                    groupedSigns =
                        ageInMonthsAtLastAssessment assembled
                            |> groupedECDSigns ageMonths
                in
                ecdSignsFromGroupedSignsByAge ageWeeks ageMonths groupedSigns
            )
        |> Maybe.withDefault []


expectedECDSignsOnMilestone : NominalDate -> NominalDate -> Maybe NominalDate -> List ECDSign
expectedECDSignsOnMilestone birthDate milestoneDate firstEncounterDateAfterMilestone =
    let
        ageWeeks =
            Date.diff Weeks birthDate milestoneDate

        ageMonths =
            Date.diff Months birthDate milestoneDate

        groupedSigns =
            Maybe.map (Date.diff Months birthDate) firstEncounterDateAfterMilestone
                |> groupedECDSigns ageMonths
    in
    ecdSignsFromGroupedSignsByAge ageWeeks ageMonths groupedSigns


ecdSignsFromGroupedSignsByAge : Int -> Int -> List (List ECDSign) -> List ECDSign
ecdSignsFromGroupedSignsByAge ageWeeks ageMonths groupedSigns =
    if ageWeeks < 5 then
        []

    else if ageWeeks < 13 then
        List.Extra.splitAt 1 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 6 then
        List.Extra.splitAt 2 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 15 then
        List.Extra.splitAt 3 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 18 then
        List.Extra.splitAt 4 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 24 then
        List.Extra.splitAt 5 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 36 then
        List.Extra.splitAt 6 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 48 then
        List.Extra.splitAt 7 groupedSigns
            |> Tuple.first
            |> List.concat

    else
        List.concat groupedSigns


groupedECDSigns : Int -> Maybe Int -> List (List ECDSign)
groupedECDSigns ageMonths ageMonthsAtLastAssessment =
    let
        ( from5Weeks, from13Weeks ) =
            Maybe.map
                (\ageMonthsLastAssessment ->
                    if ageMonthsLastAssessment >= 6 then
                        ( [], [] )

                    else
                        ( ecdSignsFrom5Weeks, ecdSignsFrom13Weeks )
                )
                ageMonthsAtLastAssessment
                |> Maybe.withDefault ( ecdSignsFrom5Weeks, ecdSignsFrom13Weeks )

        ecdSigns6To12Months =
            Maybe.map
                (\ageMonthsLastAssessment ->
                    if ageMonthsLastAssessment > 12 then
                        []

                    else if ageMonthsLastAssessment >= 9 then
                        ecdSigns6To12MonthsMajors

                    else if ageMonthsLastAssessment >= 6 then
                        if ageMonths > 12 then
                            []

                        else if ageMonths >= 9 then
                            ecdSigns6To12MonthsMajors

                        else
                            ecdSigns6To12MonthsMinors ++ ecdSigns6To12MonthsMajors

                    else
                        ecdSigns6To12MonthsMinors ++ ecdSigns6To12MonthsMajors
                )
                ageMonthsAtLastAssessment
                |> Maybe.withDefault (ecdSigns6To12MonthsMinors ++ ecdSigns6To12MonthsMajors)
    in
    [ from5Weeks
    , from13Weeks
    , ecdSigns6To12Months
    , ecdSignsFrom15Months
    , ecdSignsFrom18Months
    , ecdSignsFrom2Years
    , ecdSignsFrom3Years
    , ecdSignsFrom4Years
    ]


ageInMonthsAtLastAssessment : AssembledData -> Maybe Int
ageInMonthsAtLastAssessment assembled =
    assembled.person.birthDate
        |> Maybe.andThen
            (\birthDate ->
                let
                    lastECDAssessmentDate =
                        assembled.previousMeasurementsWithDates
                            |> List.filterMap
                                (\( date, ( _, measurements ) ) ->
                                    if isJust measurements.ecd then
                                        Just date

                                    else
                                        Nothing
                                )
                            |> List.head
                in
                Maybe.map
                    (Date.diff Months birthDate)
                    lastECDAssessmentDate
            )


ecdSignsFrom5Weeks : List ECDSign
ecdSignsFrom5Weeks =
    [ FollowMothersEyes
    , MoveArmsAndLegs
    ]


ecdSignsFrom13Weeks : List ECDSign
ecdSignsFrom13Weeks =
    [ RaiseHandsUp
    , Smile
    , RollSideways
    ]


ecdSigns6To12MonthsMinors : List ECDSign
ecdSigns6To12MonthsMinors =
    [ BringHandsToMouth
    , HoldHeadWithoutSupport
    , HoldAndShakeToys
    , ReactToSuddenSounds
    , UseConsonantSounds
    ]


ecdSigns6To12MonthsMajors : List ECDSign
ecdSigns6To12MonthsMajors =
    [ RespondToSoundWithSound
    , TurnHeadWhenCalled
    , SitWithoutSupport
    , SmileBack
    , RollTummyToBack
    , ReachForToys
    ]


ecdSignsFrom15Months : List ECDSign
ecdSignsFrom15Months =
    [ UseSimpleGestures
    , StandOnTheirOwn
    , CopyDuringPlay
    , SayMamaDada
    , CanHoldSmallObjects
    ]


ecdSignsFrom18Months : List ECDSign
ecdSignsFrom18Months =
    [ LooksWhenPointedAt
    , UseSingleWords
    , WalkWithoutHelp
    , PlayPretend
    , PointToThingsOfInterest
    ]


ecdSignsFrom2Years : List ECDSign
ecdSignsFrom2Years =
    [ UseShortPhrases
    , InterestedInOtherChildren
    , FollowSimpleInstructions
    , KickBall
    , PointAtNamedObjects
    ]


ecdSignsFrom3Years : List ECDSign
ecdSignsFrom3Years =
    [ DressThemselves
    , WashHandsGoToToiled
    , KnowsColorsAndNumbers
    , UseMediumPhrases
    , PlayMakeBelieve
    ]


ecdSignsFrom4Years : List ECDSign
ecdSignsFrom4Years =
    [ FollowThreeStepInstructions
    , StandOnOneFootFiveSeconds
    , UseLongPhrases
    , ShareWithOtherChildren
    , CountToTen
    ]


medicationTaskCompleted : NominalDate -> Site -> Bool -> AssembledData -> MedicationTask -> Bool
medicationTaskCompleted currentDate site isChw assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectMedicationTask currentDate site isChw assembled
    in
    case task of
        TaskAlbendazole ->
            (not <| taskExpected TaskAlbendazole) || isJust measurements.albendazole

        TaskMebendezole ->
            (not <| taskExpected TaskMebendezole) || isJust measurements.mebendezole

        TaskVitaminA ->
            (not <| taskExpected TaskVitaminA) || isJust measurements.vitaminA


expectMedicationTask : NominalDate -> Site -> Bool -> AssembledData -> MedicationTask -> Bool
expectMedicationTask currentDate site isChw assembled task =
    let
        nextAdmnistrationData =
            getPreviousMeasurements assembled.previousMeasurementsWithDates
                |> nextMedicationAdmnistrationData currentDate site assembled.person
    in
    Dict.get task nextAdmnistrationData
        |> Maybe.map
            (\nextDate ->
                let
                    compare =
                        Date.compare nextDate currentDate
                in
                compare == LT || compare == EQ
            )
        |> Maybe.withDefault False


nextMedicationAdmnistrationData : NominalDate -> Site -> Person -> List WellChildMeasurements -> Dict MedicationTask NominalDate
nextMedicationAdmnistrationData currentDate site person measurements =
    List.filter (expectMedicationByAge currentDate site person) medicationTasks
        |> List.map
            (\medication ->
                case medication of
                    TaskAlbendazole ->
                        List.filterMap .albendazole measurements
                            |> latestAdministrationDateForMedicine
                            |> Maybe.map (\date -> ( TaskAlbendazole, Date.add Months 6 date ))
                            |> Maybe.withDefault ( TaskAlbendazole, currentDate )

                    TaskMebendezole ->
                        List.filterMap .mebendezole measurements
                            |> latestAdministrationDateForMedicine
                            |> Maybe.map (\date -> ( TaskMebendezole, Date.add Months 6 date ))
                            |> Maybe.withDefault ( TaskMebendezole, currentDate )

                    TaskVitaminA ->
                        List.filterMap .vitaminA measurements
                            |> latestAdministrationDateForMedicine
                            |> Maybe.map (\date -> ( TaskVitaminA, Date.add Months 6 date ))
                            |> Maybe.withDefault ( TaskVitaminA, currentDate )
            )
        |> Dict.fromList


expectMedicationByAge : NominalDate -> Site -> Person -> MedicationTask -> Bool
expectMedicationByAge currentDate site person task =
    ageInMonths currentDate person
        |> Maybe.map
            (\ageMonths ->
                case site of
                    SiteBurundi ->
                        case task of
                            -- 6 months to 12 years.
                            TaskAlbendazole ->
                                ageMonths >= 6 && ageMonths < (12 * 12)

                            -- Never.
                            TaskMebendezole ->
                                False

                            -- 6 months to 6 years.
                            TaskVitaminA ->
                                ageMonths >= 6 && ageMonths < (6 * 6)

                    _ ->
                        case task of
                            -- 6 years to 12 years.
                            TaskAlbendazole ->
                                ageMonths >= (6 * 12) && ageMonths < (12 * 12)

                            -- 1 year to 6 years.
                            TaskMebendezole ->
                                ageMonths >= 12 && ageMonths < (6 * 12)

                            -- 6 months to 6 years.
                            TaskVitaminA ->
                                ageMonths >= 6 && ageMonths < (6 * 12)
            )
        |> Maybe.withDefault False


latestAdministrationDateForMedicine : List ( id, { a | value : AdministrationNote, dateMeasured : NominalDate } ) -> Maybe NominalDate
latestAdministrationDateForMedicine measurements =
    List.filterMap
        (Tuple.second
            >> (\measurement ->
                    if measurement.value == AdministeredToday then
                        Just measurement.dateMeasured

                    else
                        Nothing
               )
        )
        measurements
        |> List.head


medicationTasksCompletedFromTotal : NominalDate -> Site -> AssembledData -> MedicationData -> MedicationTask -> ( Int, Int )
medicationTasksCompletedFromTotal currentDate site assembled data task =
    let
        measurements =
            assembled.measurements

        ( _, tasks ) =
            case task of
                TaskAlbendazole ->
                    let
                        config =
                            { medication = Albendazole
                            , setMedicationAdministeredMsg = SetAlbendazoleAdministered
                            , setReasonForNonAdministration = SetAlbendazoleReasonForNonAdministration
                            , resolveDosageAndIconFunc = resolveAlbendazoleDosageAndIcon
                            , helper = Translate.AdministerAlbendazoleHelper
                            }
                    in
                    getMeasurementValueFunc measurements.albendazole
                        |> medicationAdministrationFormWithDefault data.albendazoleForm
                        |> medicationAdministrationFormInputsAndTasks English currentDate site assembled config

                TaskMebendezole ->
                    let
                        config =
                            { medication = Mebendezole
                            , setMedicationAdministeredMsg = SetMebendezoleAdministered
                            , setReasonForNonAdministration = SetMebendezoleReasonForNonAdministration
                            , resolveDosageAndIconFunc = resolveMebendezoleDosageAndIcon
                            , helper = Translate.AdministerMebendezoleHelper
                            }
                    in
                    getMeasurementValueFunc measurements.mebendezole
                        |> medicationAdministrationFormWithDefault data.mebendezoleForm
                        |> medicationAdministrationFormInputsAndTasks English currentDate site assembled config

                TaskVitaminA ->
                    let
                        config =
                            { medication = VitaminA
                            , setMedicationAdministeredMsg = SetVitaminAAdministered
                            , setReasonForNonAdministration = SetVitaminAReasonForNonAdministration
                            , resolveDosageAndIconFunc = resolveVitaminADosageAndIcon
                            , helper = Translate.AdministerVitaminAHelperWellChild
                            }
                    in
                    getMeasurementValueFunc measurements.vitaminA
                        |> medicationAdministrationFormWithDefault data.vitaminAForm
                        |> medicationAdministrationFormInputsAndTasks English currentDate site assembled config
    in
    resolveTasksCompletedFromTotal tasks


medicationAdministrationFormInputsAndTasks :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> MedicationAdministrationFormConfig
    -> MedicationAdministrationForm
    -> ( List (Html Msg), List (Maybe Bool) )
medicationAdministrationFormInputsAndTasks language currentDate site assembled config form =
    let
        instructions =
            config.resolveDosageAndIconFunc currentDate site assembled.person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign config.medication) icon dosage
                            , div [ class "prescription" ] [ text <| translate language config.helper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        questions =
            concatInputsAndTasksSections
                [ ( [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign config.medication)
                    , viewBoolInput
                        language
                        form.medicationAdministered
                        config.setMedicationAdministeredMsg
                        ""
                        Nothing
                    ]
                  , [ form.medicationAdministered ]
                  )
                , derivedQuestion
                ]

        derivedQuestion =
            if form.medicationAdministered == Just False then
                ( [ viewQuestionLabel language Translate.WhyNot
                  , viewCheckBoxSelectInput language
                        [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
                        [ NonAdministrationPatientDeclined, NonAdministrationOther ]
                        form.reasonForNonAdministration
                        config.setReasonForNonAdministration
                        Translate.AdministrationNote
                  ]
                , [ maybeToBoolTask form.reasonForNonAdministration ]
                )

            else
                ( [], [] )
    in
    concatInputsAndTasksSections
        [ ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
            , instructions
            ]
          , []
          )
        , questions
        ]


viewAdministeredMedicationLabel : Language -> TranslationId -> TranslationId -> String -> String -> Html any
viewAdministeredMedicationLabel language administerTranslationId medicineTranslationId iconClass dosage =
    viewAdministeredMedicationCustomLabel language administerTranslationId medicineTranslationId "" iconClass dosage Nothing


fromAdministrationNote : Maybe AdministrationNote -> MedicationAdministrationForm
fromAdministrationNote saved =
    Maybe.map
        (\administrationNote ->
            let
                ( medicationAdministered, reasonForNonAdministration ) =
                    if administrationNote == AdministeredToday then
                        ( Just True, Nothing )

                    else
                        ( Just False, Just administrationNote )
            in
            MedicationAdministrationForm medicationAdministered reasonForNonAdministration
        )
        saved
        |> Maybe.withDefault emptyMedicationAdministrationForm


medicationAdministrationFormWithDefault : MedicationAdministrationForm -> Maybe AdministrationNote -> MedicationAdministrationForm
medicationAdministrationFormWithDefault form saved =
    let
        fromSavedForm =
            fromAdministrationNote saved
    in
    { medicationAdministered = or form.medicationAdministered fromSavedForm.medicationAdministered
    , reasonForNonAdministration = or form.reasonForNonAdministration fromSavedForm.reasonForNonAdministration
    }


toAdministrationNoteWithDefault : Maybe AdministrationNote -> MedicationAdministrationForm -> Maybe AdministrationNote
toAdministrationNoteWithDefault saved form =
    medicationAdministrationFormWithDefault form saved
        |> toAdministrationNote


toAdministrationNote : MedicationAdministrationForm -> Maybe AdministrationNote
toAdministrationNote form =
    form.medicationAdministered
        |> Maybe.andThen
            (\medicationAdministered ->
                if medicationAdministered then
                    Just AdministeredToday

                else
                    form.reasonForNonAdministration
            )


resolveAlbendazoleDosageAndIcon : NominalDate -> Site -> Person -> Maybe ( String, String )
resolveAlbendazoleDosageAndIcon currentDate site person =
    case site of
        SiteBurundi ->
            ageInMonths currentDate person
                |> Maybe.map
                    (\ageMonths ->
                        if ageMonths < 24 then
                            ( "200 mg", "icon-pills" )

                        else
                            ( "400 mg", "icon-pills" )
                    )

        _ ->
            Just ( "400 mg", "icon-pills" )


resolveMebendezoleDosageAndIcon : NominalDate -> Site -> Person -> Maybe ( String, String )
resolveMebendezoleDosageAndIcon currentDate site person =
    case site of
        SiteBurundi ->
            Nothing

        _ ->
            Just ( "500 mg", "icon-pills" )


resolveVitaminADosageAndIcon : NominalDate -> Site -> Person -> Maybe ( String, String )
resolveVitaminADosageAndIcon currentDate site person =
    ageInMonths currentDate person
        |> Maybe.map
            (\ageMonths ->
                case site of
                    SiteBurundi ->
                        if ageMonths < 12 then
                            ( "100,000 IU", "icon-capsule blue" )

                        else
                            ( "200,000 IU", "icon-capsule red" )

                    _ ->
                        if ageMonths < 18 then
                            ( "100,000 IU", "icon-capsule blue" )

                        else
                            ( "200,000 IU", "icon-capsule red" )
            )


nextStepsTaskCompleted :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> Pages.WellChild.Activity.Types.NextStepsTask
    -> Bool
nextStepsTaskCompleted currentDate zscores site features isChw data db task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectNextStepsTask currentDate zscores site features isChw data db
    in
    case task of
        TaskContributingFactors ->
            (not <| taskExpected TaskContributingFactors) || isJust measurements.contributingFactors

        TaskHealthEducation ->
            (not <| taskExpected TaskHealthEducation) || isJust measurements.healthEducation

        TaskFollowUp ->
            (not <| taskExpected TaskFollowUp) || isJust measurements.followUp

        TaskSendToHC ->
            (not <| taskExpected TaskSendToHC) || isJust measurements.sendToHC

        TaskNextVisit ->
            (not <| taskExpected TaskNextVisit)
                || isJust measurements.nextVisit


expectNextStepsTask :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> Pages.WellChild.Activity.Types.NextStepsTask
    -> Bool
expectNextStepsTask currentDate zscores site features isChw assembled db task =
    case task of
        TaskContributingFactors ->
            if mandatoryNutritionAssessmentTasksCompleted currentDate assembled then
                -- Any assesment requires Next Steps tasks.
                generateNutritionAssessment currentDate zscores db assembled
                    |> List.isEmpty
                    |> not

            else
                False

        TaskHealthEducation ->
            expectNextStepsTask currentDate zscores site features isChw assembled db TaskContributingFactors
                || -- CHW should send patient to HC, if child is behind on vaccinatons.
                   ((assembled.encounter.encounterType /= PediatricCare)
                        && activityCompleted currentDate zscores site features isChw assembled db WellChildImmunisation
                        && isBehindOnVaccinationsByProgress currentDate site assembled.participant.person db
                   )

        TaskFollowUp ->
            expectNextStepsTask currentDate zscores site features isChw assembled db TaskContributingFactors

        TaskSendToHC ->
            expectNextStepsTask currentDate zscores site features isChw assembled db TaskContributingFactors
                || -- CHW should send patient to HC, if child is behind on vaccinatons.
                   ((assembled.encounter.encounterType /= PediatricCare)
                        && activityCompleted currentDate zscores site features isChw assembled db WellChildImmunisation
                        && isBehindOnVaccinationsByProgress currentDate site assembled.participant.person db
                   )

        TaskNextVisit ->
            activityCompleted currentDate zscores site features isChw assembled db WellChildNutritionAssessment
                -- Activities that affect determinating next visit date are
                -- either completed, or not shown at current visit.
                && activityCompleted currentDate zscores site features isChw assembled db WellChildImmunisation
                && activityCompleted currentDate zscores site features isChw assembled db WellChildECD
                && activityCompleted currentDate zscores site features isChw assembled db WellChildMedication
                && nextVisitRequired currentDate site assembled db


immunisationTasksCompletedFromTotal :
    Language
    -> NominalDate
    -> Site
    -> Bool
    -> AssembledData
    -> ImmunisationData
    -> Measurement.Model.ImmunisationTask
    -> ( Int, Int )
immunisationTasksCompletedFromTotal language currentDate site isChw assembled data task =
    Maybe.map
        (\vaccineType ->
            let
                form =
                    case vaccineType of
                        VaccineBCG ->
                            assembled.measurements.bcgImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.bcgForm

                        VaccineDTP ->
                            assembled.measurements.dtpImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.dtpForm

                        VaccineDTPStandalone ->
                            assembled.measurements.dtpStandaloneImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.dtpStandaloneForm

                        VaccineHPV ->
                            assembled.measurements.hpvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.hpvForm

                        VaccineIPV ->
                            assembled.measurements.ipvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.ipvForm

                        VaccineMR ->
                            assembled.measurements.mrImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.mrForm

                        VaccineOPV ->
                            assembled.measurements.opvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.opvForm

                        VaccinePCV13 ->
                            assembled.measurements.pcv13Immunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.pcv13Form

                        VaccineRotarix ->
                            assembled.measurements.rotarixImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.rotarixForm

                ( _, tasksActive, tasksCompleted ) =
                    vaccinationFormDynamicContentAndTasks language currentDate site isChw assembled vaccineType form
            in
            ( tasksActive, tasksCompleted )
        )
        (immunisationTaskToVaccineType task)
        |> Maybe.withDefault ( 0, 0 )


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> Site
    -> Bool
    -> AssembledData
    -> WellChildVaccineType
    -> WellChildVaccinationForm
    -> ( List (Html Msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate site isChw assembled vaccineType form =
    Maybe.map
        (\birthDate ->
            let
                config =
                    { birthDate = birthDate
                    , expectedDoses = expectedDoses
                    , dosesFromPreviousEncountersData = dosesFromPreviousEncountersData
                    , dosesFromCurrentEncounterData = dosesFromCurrentEncounterData
                    , setVaccinationFormViewModeMsg = SetVaccinationFormViewMode vaccineType
                    , setUpdatePreviousVaccinesMsg = SetUpdatePreviousVaccines vaccineType
                    , setWillReceiveVaccineTodayMsg = SetWillReceiveVaccineToday vaccineType
                    , setAdministrationNoteMsg = SetAdministrationNote vaccineType
                    , setVaccinationUpdateDateSelectorStateMsg = SetVaccinationUpdateDateSelectorState vaccineType
                    , setVaccinationUpdateDateMsg = SetVaccinationUpdateDate vaccineType
                    , saveVaccinationUpdateDateMsg = SaveVaccinationUpdateDate vaccineType
                    , deleteVaccinationUpdateDateMsg = DeleteVaccinationUpdateDate vaccineType
                    , nextVaccinationDataForVaccine = nextVaccinationDataForVaccine site assembled.person.birthDate vaccineType initialOpvAdministered
                    , getIntervalForVaccine = always (getIntervalForVaccine site vaccineType)
                    , firstDoseExpectedFrom =
                        initialVaccinationDateByBirthDate site
                            birthDate
                            initialOpvAdministered
                            assembled.vaccinationProgress
                            ( vaccineType, VaccineDoseFirst )

                    -- Only nurses at HC can administer vaccinations.
                    -- CHWs only record previous vaccinations given by nurses.
                    , suggestDoseToday = assembled.encounter.encounterType == PediatricCare
                    }

                initialOpvAdministeredByForm =
                    wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm birthDate form

                initialOpvAdministered =
                    if form.administeredDosesDirty then
                        initialOpvAdministeredByForm

                    else
                        let
                            initialOpvAdministeredByProgress =
                                wasInitialOpvAdministeredByVaccinationProgress assembled.person.birthDate assembled.vaccinationProgress
                        in
                        initialOpvAdministeredByForm || initialOpvAdministeredByProgress

                expectedDoses =
                    getAllDosesForVaccine initialOpvAdministered vaccineType
                        |> List.filter
                            (\dose ->
                                expectVaccineDoseForPerson currentDate
                                    site
                                    assembled.person
                                    initialOpvAdministered
                                    assembled.vaccinationProgress
                                    ( vaccineType, dose )
                            )

                dosesFromPreviousEncountersData =
                    Dict.get vaccineType assembled.vaccinationHistory
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList

                dosesFromCurrentEncounterData =
                    Maybe.map2
                        (\doses dates ->
                            let
                                orderedDoses =
                                    EverySet.toList doses
                                        |> List.sortBy vaccineDoseToComparable

                                orderedDates =
                                    EverySet.toList dates
                                        |> List.sortWith Date.compare
                            in
                            List.Extra.zip orderedDoses orderedDates
                        )
                        form.administeredDoses
                        form.administrationDates
                        |> Maybe.withDefault []
            in
            Measurement.Utils.vaccinationFormDynamicContentAndTasks language
                currentDate
                site
                config
                (WellChildVaccine vaccineType)
                form
        )
        assembled.person.birthDate
        |> Maybe.withDefault ( [], 0, 1 )


nextStepsTasksCompletedFromTotal : NominalDate -> Bool -> WellChildMeasurements -> NextStepsData -> Pages.WellChild.Activity.Types.NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal currentDate isChw measurements data task =
    case task of
        TaskContributingFactors ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc measurements.contributingFactors
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> contributingFactorsFormInutsAndTasks English
                            currentDate
                            Pages.WellChild.Activity.Model.SetContributingFactorsSign
            in
            resolveTasksCompletedFromTotal tasks

        TaskHealthEducation ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInutsAndTasks English
                            currentDate
                            Pages.WellChild.Activity.Model.SetProvidedEducationForDiagnosis
                            Pages.WellChild.Activity.Model.SetReasonForNotProvidingHealthEducation
            in
            resolveTasksCompletedFromTotal tasks

        TaskFollowUp ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc measurements.followUp
                        |> nutritionFollowUpFormWithDefault data.followUpForm
                        |> followUpFormInputsAndTasks English
                            currentDate
                            []
                            Pages.WellChild.Activity.Model.SetFollowUpOption
            in
            resolveTasksCompletedFromTotal tasks

        TaskSendToHC ->
            let
                form =
                    getMeasurementValueFunc measurements.sendToHC
                        |> sendToHCFormWithDefault data.sendToHCForm

                ( _, tasks ) =
                    if isChw then
                        sendToFacilityInputsAndTasks English
                            currentDate
                            FacilityHealthCenter
                            Pages.WellChild.Activity.Model.SetReferToHealthCenter
                            Pages.WellChild.Activity.Model.SetReasonForNonReferral
                            Pages.WellChild.Activity.Model.SetHandReferralForm
                            Nothing
                            form

                    else
                        referToProgramFormInputsAndTasks English
                            currentDate
                            Pages.WellChild.Activity.Model.SetEnrollToNutritionProgram
                            Pages.WellChild.Activity.Model.SetReferToNutritionProgram
                            form
            in
            resolveTasksCompletedFromTotal tasks

        TaskNextVisit ->
            let
                form =
                    getMeasurementValueFunc measurements.nextVisit
                        |> nextVisitFormWithDefault data.nextVisitForm
            in
            ( taskAnyCompleted [ form.immunisationDate, form.pediatricVisitDate ]
            , 1
            )


nextStepsTasks : List Pages.WellChild.Activity.Types.NextStepsTask
nextStepsTasks =
    [ TaskContributingFactors, TaskHealthEducation, TaskSendToHC, TaskFollowUp, TaskNextVisit ]


nextVisitRequired : NominalDate -> Site -> AssembledData -> ModelIndexedDb -> Bool
nextVisitRequired currentDate site assembled db =
    let
        ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
            generateNextVisitDates currentDate site assembled db
    in
    isJust nextDateForImmunisationVisit || isJust nextDateForPediatricVisit


generateNextVisitDates : NominalDate -> Site -> AssembledData -> ModelIndexedDb -> ( Maybe NominalDate, Maybe NominalDate )
generateNextVisitDates currentDate site assembled db =
    let
        nextVisitDateForECD =
            generateNextDateForECDVisit currentDate assembled db

        nextVisitDateForMedication =
            generateNextDateForMedicationVisit currentDate site assembled db
    in
    ( generateNextDateForImmunisationVisit currentDate site assembled
    , Maybe.Extra.values [ nextVisitDateForECD, nextVisitDateForMedication ]
        |> List.sortWith Date.compare
        |> List.head
    )


generateNextDateForECDVisit : NominalDate -> AssembledData -> ModelIndexedDb -> Maybe NominalDate
generateNextDateForECDVisit currentDate assembled db =
    assembled.person.birthDate
        |> Maybe.andThen
            (\birthDate ->
                let
                    ageWeeks =
                        Date.diff Weeks birthDate currentDate

                    noRemainingSigns =
                        List.isEmpty <| generateRemianingECDSignsAfterCurrentEncounter currentDate assembled
                in
                if ageWeeks < 6 then
                    -- Since 6 weeks question appear from age of 5 weeks,
                    -- we check if they were completed then.
                    -- If so, we schedule next ECD visit to following
                    -- milestone, which is at 14 weeks.
                    if ageWeeks == 5 && noRemainingSigns then
                        Just <| Date.add Weeks 14 birthDate

                    else
                        Just <| Date.add Weeks 6 birthDate

                else if ageWeeks < 14 then
                    -- Since 14 weeks question appear from age of 13 weeks,
                    -- we check if they were completed then.
                    -- If so, we schedule next ECD visit to following
                    -- milestone, which is at 6 months.
                    if ageWeeks == 13 && noRemainingSigns then
                        Just <| Date.add Months 6 birthDate

                    else
                        Just <| Date.add Weeks 14 birthDate

                else
                    let
                        ageMonths =
                            Date.diff Months birthDate currentDate
                    in
                    if ageMonths < 6 then
                        Just <| Date.add Months 6 birthDate

                    else if ageMonths < 15 then
                        Just <| Date.add Months 15 birthDate

                    else
                        let
                            ageYears =
                                Date.diff Years birthDate currentDate
                        in
                        if ageYears < 2 then
                            Just <| Date.add Years 2 birthDate

                        else if ageYears < 3 then
                            Just <| Date.add Years 3 birthDate

                        else if ageYears < 4 then
                            Just <| Date.add Years 4 birthDate

                        else if not noRemainingSigns then
                            Just <| Date.add Months 6 currentDate

                        else
                            Nothing
            )


generateNextDateForMedicationVisit : NominalDate -> Site -> AssembledData -> ModelIndexedDb -> Maybe NominalDate
generateNextDateForMedicationVisit currentDate site assembled db =
    assembled.person.birthDate
        |> Maybe.andThen
            (\birthDate ->
                let
                    ageMonths =
                        Date.diff Months birthDate currentDate
                in
                -- When younger than 6 months, set visit date to
                -- age of 6 months.
                if ageMonths < 6 then
                    Just <| Date.add Months 6 birthDate

                else
                    let
                        measurements =
                            assembled.measurements :: getPreviousMeasurements assembled.previousMeasurementsWithDates

                        nextDate =
                            nextMedicationAdmnistrationData currentDate site assembled.person measurements
                                |> Dict.values
                                |> List.sortWith Date.compare
                                |> List.reverse
                                |> List.head
                    in
                    Maybe.map
                        (\date ->
                            let
                                compared =
                                    Date.compare date currentDate
                            in
                            if compared == LT || compared == EQ then
                                -- Next date already passed, or, it's due today.
                                -- Per requirements, we schedule next date as if medication
                                -- was administered today.
                                Date.add Months 6 currentDate

                            else
                                date
                        )
                        nextDate
            )


generateNextDateForImmunisationVisit : NominalDate -> Site -> AssembledData -> Maybe NominalDate
generateNextDateForImmunisationVisit currentDate site assembled =
    let
        futureVaccinationsData =
            generateFutureVaccinationsData currentDate site assembled.person.birthDate assembled.person.gender True assembled.vaccinationProgress

        -- If there're only 6 months interval vaccines (which are given at older age),
        -- we'll suggested most recent date.
        -- Otherwise, there's a vaccine with 28 days interval, so, per requirements,
        -- when there are several vaccines, we select the latest date - to be able to
        -- administer all in a single encounter.
        -- We do not want to wait 6 months, if we need to administer a vaccine
        -- that needs only 28 days interval.
        ( longIntervalVaccinesData, shortIntervalVaccinesData ) =
            List.partition
                (\( vaccineType, _ ) ->
                    List.member vaccineType [ VaccineMR, VaccineHPV ]
                )
                futureVaccinationsData

        ( nextVisitDate, interval, unit ) =
            if List.isEmpty shortIntervalVaccinesData then
                ( List.filterMap (Tuple.second >> Maybe.map Tuple.second) longIntervalVaccinesData
                    |> List.sortWith Date.compare
                    -- Get the most recent of all dates.
                    |> List.head
                , 6
                , Months
                )

            else
                ( List.filterMap (Tuple.second >> Maybe.map Tuple.second) shortIntervalVaccinesData
                    |> List.filter
                        -- There can be a situation where IPV vaccine is to
                        -- be administeredon latter date (first given at 14 weeks).
                        -- We avoid this situation, and consider only dates that
                        -- are due withing a month.
                        (\administrationDate ->
                            Date.diff Days currentDate administrationDate < 30
                        )
                    |> List.sortWith Date.compare
                    -- Get the latest of all dates.
                    |> List.reverse
                    |> List.head
                , 28
                , Days
                )
    in
    -- If we see that next suggested date already passed, or is set for today,
    -- oer requirements, we set next visit to 1 vaccine interval from current date.
    Maybe.map
        (\nextDate ->
            if Date.compare nextDate currentDate /= GT then
                Date.add unit interval currentDate

            else
                nextDate
        )
        nextVisitDate


{-| The purpose here is to find ou if patient is behind on immunisations.
If so, what was the date from which the lag started.
If not, on which date we'll need to administer next vaccination (of any type).
-}
generateASAPImmunisationDate : NominalDate -> Site -> AssembledData -> Maybe NominalDate
generateASAPImmunisationDate currentDate site assembled =
    generateFutureVaccinationsData currentDate
        site
        assembled.person.birthDate
        assembled.person.gender
        False
        assembled.vaccinationProgress
        |> List.filterMap (Tuple.second >> Maybe.map Tuple.second)
        |> List.sortWith Date.compare
        -- Get the most recent of all dates.
        |> List.head


fromNextVisitValue : Maybe NextVisitValue -> NextVisitForm
fromNextVisitValue saved =
    { immunisationDate = Maybe.andThen .immunisationDate saved
    , asapImmunisationDate = Maybe.andThen .asapImmunisationDate saved
    , pediatricVisitDate = Maybe.andThen .pediatricVisitDate saved
    , resolutionDate = Maybe.andThen .resolutionDate saved
    }


nextVisitFormWithDefault : NextVisitForm -> Maybe NextVisitValue -> NextVisitForm
nextVisitFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { immunisationDate = or form.immunisationDate value.immunisationDate
                , asapImmunisationDate = or form.asapImmunisationDate value.asapImmunisationDate
                , pediatricVisitDate = or form.pediatricVisitDate value.pediatricVisitDate
                , resolutionDate = or form.resolutionDate value.resolutionDate
                }
            )


toNextVisitValueWithDefault : Maybe NextVisitValue -> NextVisitForm -> Maybe NextVisitValue
toNextVisitValueWithDefault saved form =
    nextVisitFormWithDefault form saved
        |> toNextVisitValue


toNextVisitValue : NextVisitForm -> Maybe NextVisitValue
toNextVisitValue form =
    Just <|
        NextVisitValue
            form.immunisationDate
            form.asapImmunisationDate
            form.pediatricVisitDate
            form.resolutionDate



-- HELPER FUNCTIONS


resolvePreviousValue : AssembledData -> (WellChildMeasurements -> Maybe ( id, WellChildMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (Tuple.second
                >> Tuple.second
                >> measurementFunc
                >> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head
