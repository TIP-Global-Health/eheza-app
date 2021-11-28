module Pages.WellChildActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (WellChildEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, headCircumferenceValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (..)
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue, ifTrue, taskAnyCompleted, taskCompleted, valueConsideringIsDirtyField)
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildEncounter.Model exposing (AssembledData, VaccinationProgressDict)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)
import ZScore.Model exposing (Kilograms(..))
import ZScore.Utils exposing (zScoreWeightForAge)


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


activityCompleted : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
activityCompleted currentDate zscores isChw assembled db activity =
    let
        measurements =
            assembled.measurements

        activityExpected =
            expectActivity currentDate zscores isChw assembled db
    in
    case activity of
        WellChildPregnancySummary ->
            (not <| activityExpected WellChildPregnancySummary)
                || isJust measurements.pregnancySummary

        WellChildDangerSigns ->
            (not <| activityExpected WellChildDangerSigns)
                || (isJust measurements.symptomsReview && isJust measurements.vitals)

        WellChildNutritionAssessment ->
            resolveNutritionAssessmentTasks isChw
                |> List.all (nutritionAssessmentTaskCompleted currentDate isChw assembled db)

        WellChildECD ->
            (not <| activityExpected WellChildECD) || isJust measurements.ecd

        WellChildMedication ->
            (not <| activityExpected WellChildMedication)
                || List.all (medicationTaskCompleted currentDate isChw assembled) medicationTasks

        WellChildImmunisation ->
            (not <| activityExpected WellChildImmunisation)
                || List.all (immunisationTaskCompleted currentDate isChw assembled db) immunisationVaccinationTasks

        WellChildNextSteps ->
            List.all (nextStepsTaskCompleted currentDate zscores isChw assembled db) nextStepsTasks

        WellChildPhoto ->
            (not <| activityExpected WellChildPhoto) || isJust measurements.photo


expectActivity : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
expectActivity currentDate zscores isChw assembled db activity =
    case activity of
        WellChildPregnancySummary ->
            if isChw then
                ageInMonths currentDate assembled.person
                    |> Maybe.map
                        (\ageMonths -> ageMonths < 2)
                    |> Maybe.withDefault False

            else
                False

        WellChildDangerSigns ->
            not isChw

        WellChildNutritionAssessment ->
            True

        WellChildImmunisation ->
            generateSuggestedVaccinations currentDate isChw assembled
                |> List.isEmpty
                |> not

        WellChildECD ->
            if isChw then
                False

            else
                generateRemianingECDSignsBeforeCurrentEncounter currentDate assembled
                    |> List.isEmpty
                    |> not

        WellChildMedication ->
            if isChw then
                False

            else
                medicationTasks
                    |> List.filter (expectMedicationTask currentDate isChw assembled)
                    |> List.isEmpty
                    |> not

        WellChildNextSteps ->
            nextStepsTasks
                |> List.filter (expectNextStepsTask currentDate zscores isChw assembled db)
                |> List.isEmpty
                |> not

        WellChildPhoto ->
            True


fromPregnancySummaryValue : Maybe PregnancySummaryValue -> PregnancySummaryForm
fromPregnancySummaryValue saved =
    let
        deliveryComplications =
            Maybe.map (.deliveryComplications >> EverySet.toList) saved

        deliveryComplicationsPresent =
            Maybe.map complicationsPresent deliveryComplications
    in
    { expectedDateConcluded = Maybe.map .expectedDateConcluded saved
    , isExpectedDateConcludedSelectorOpen = False
    , deliveryComplicationsPresent = deliveryComplicationsPresent
    , deliveryComplications = deliveryComplications
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
                in
                { expectedDateConcluded = or form.expectedDateConcluded (Just value.expectedDateConcluded)
                , isExpectedDateConcludedSelectorOpen = form.isExpectedDateConcludedSelectorOpen
                , deliveryComplicationsPresent = or form.deliveryComplicationsPresent (complicationsPresent deliveryComplications |> Just)
                , deliveryComplications = or form.deliveryComplications (Just deliveryComplications)
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
            form.deliveryComplications
                |> Maybe.map EverySet.fromList
                |> Maybe.withDefault (EverySet.singleton NoDeliveryComplications)
    in
    Maybe.map PregnancySummaryValue form.expectedDateConcluded
        |> andMap (Just deliveryComplications)


complicationsPresent : List DeliveryComplication -> Bool
complicationsPresent complications =
    case complications of
        [] ->
            False

        [ NoDeliveryComplications ] ->
            False

        _ ->
            True


nutritionAssessmentTaskCompleted : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> NutritionAssessmentTask -> Bool
nutritionAssessmentTaskCompleted currentDate isChw data db task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectNutritionAssessmentTask currentDate isChw data db
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


expectNutritionAssessmentTask : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> NutritionAssessmentTask -> Bool
expectNutritionAssessmentTask currentDate isChw data db task =
    case task of
        -- Show for children that are up to 3 years old.
        TaskHeadCircumference ->
            ageInMonths currentDate data.person
                |> Maybe.map (\ageMonths -> ageMonths < 36)
                |> Maybe.withDefault False

        -- Show for children that are at least 6 month old.
        TaskMuac ->
            ageInMonths currentDate data.person
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        -- View any other task.
        _ ->
            True


mandatoryNutritionAssessmentTasksCompleted : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> Bool
mandatoryNutritionAssessmentTasksCompleted currentDate isChw data db =
    resolveNutritionAssessmentTasks isChw
        |> List.filter (not << nutritionAssessmentTaskCompleted currentDate isChw data db)
        |> List.isEmpty


resolveNutritionAssessmentTasks : Bool -> List NutritionAssessmentTask
resolveNutritionAssessmentTasks isChw =
    if isChw then
        -- Height and Muac are not here, because Newbor Exam
        -- is done for children that are less than 2 months old.
        [ TaskHeadCircumference, TaskNutrition, TaskWeight ]

    else
        [ TaskHeight, TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ]


nutritionAssessmentTasksCompletedFromTotal : WellChildMeasurements -> NutritionAssessmentData -> NutritionAssessmentTask -> ( Int, Int )
nutritionAssessmentTasksCompletedFromTotal measurements data task =
    case task of
        TaskHeight ->
            let
                form =
                    measurements.height
                        |> getMeasurementValueFunc
                        |> heightFormWithDefault data.heightForm
            in
            ( taskCompleted form.height
            , 1
            )

        TaskHeadCircumference ->
            let
                form =
                    measurements.headCircumference
                        |> getMeasurementValueFunc
                        |> headCircumferenceFormWithDefault data.headCircumferenceForm
            in
            ( taskCompleted form.headCircumference
            , 1
            )

        TaskMuac ->
            let
                form =
                    measurements.muac
                        |> getMeasurementValueFunc
                        |> muacFormWithDefault data.muacForm
            in
            ( taskCompleted form.muac
            , 1
            )

        TaskNutrition ->
            let
                form =
                    measurements.nutrition
                        |> getMeasurementValueFunc
                        |> nutritionFormWithDefault data.nutritionForm
            in
            ( taskCompleted form.signs
            , 1
            )

        TaskWeight ->
            let
                form =
                    measurements.weight
                        |> getMeasurementValueFunc
                        |> weightFormWithDefault data.weightForm
            in
            ( taskCompleted form.weight
            , 1
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


fromWellChildECDValue : Maybe (EverySet ECDSign) -> WellChildECDForm
fromWellChildECDValue signs =
    { followMothersEyes = Maybe.map (EverySet.member FollowMothersEyes) signs
    , moveArmsAndLegs = Maybe.map (EverySet.member MoveArmsAndLegs) signs
    , raiseHandsUp = Maybe.map (EverySet.member RaiseHandsUp) signs
    , smile = Maybe.map (EverySet.member Smile) signs
    , rollSideways = Maybe.map (EverySet.member RollSideways) signs
    , bringHandsToMouth = Maybe.map (EverySet.member BringHandsToMouth) signs
    , holdHeadWithoutSupport = Maybe.map (EverySet.member HoldHeadWithoutSupport) signs
    , holdAndShakeToys = Maybe.map (EverySet.member HoldAndShakeToys) signs
    , reactToSuddenSounds = Maybe.map (EverySet.member ReactToSuddenSounds) signs
    , useConsonantSounds = Maybe.map (EverySet.member UseConsonantSounds) signs
    , respondToSoundWithSound = Maybe.map (EverySet.member RespondToSoundWithSound) signs
    , turnHeadWhenCalled = Maybe.map (EverySet.member TurnHeadWhenCalled) signs
    , sitWithoutSupport = Maybe.map (EverySet.member SitWithoutSupport) signs
    , smileBack = Maybe.map (EverySet.member SmileBack) signs
    , rollTummyToBack = Maybe.map (EverySet.member RollTummyToBack) signs
    , reachForToys = Maybe.map (EverySet.member ReachForToys) signs
    , useSimpleGestures = Maybe.map (EverySet.member UseSimpleGestures) signs
    , standOnTheirOwn = Maybe.map (EverySet.member StandOnTheirOwn) signs
    , copyDuringPlay = Maybe.map (EverySet.member CopyDuringPlay) signs
    , sayMamaDada = Maybe.map (EverySet.member SayMamaDada) signs
    , canHoldSmallObjects = Maybe.map (EverySet.member CanHoldSmallObjects) signs
    , looksWhenPointedAt = Maybe.map (EverySet.member LooksWhenPointedAt) signs
    , useSingleWords = Maybe.map (EverySet.member UseSingleWords) signs
    , walkWithoutHelp = Maybe.map (EverySet.member WalkWithoutHelp) signs
    , playPretend = Maybe.map (EverySet.member PlayPretend) signs
    , pointToThingsOfInterest = Maybe.map (EverySet.member PointToThingsOfInterest) signs
    , useShortPhrases = Maybe.map (EverySet.member UseShortPhrases) signs
    , interestedInOtherChildren = Maybe.map (EverySet.member InterestedInOtherChildren) signs
    , followSimlpeInstructions = Maybe.map (EverySet.member FollowSimpleInstructions) signs
    , kickBall = Maybe.map (EverySet.member KickBall) signs
    , pointAtNamedObjects = Maybe.map (EverySet.member PointAtNamedObjects) signs
    , dressThemselves = Maybe.map (EverySet.member DressThemselves) signs
    , washHandsGoToToiled = Maybe.map (EverySet.member WashHandsGoToToiled) signs
    , knowsColorsAndNumbers = Maybe.map (EverySet.member KnowsColorsAndNumbers) signs
    , useMediumPhrases = Maybe.map (EverySet.member UseMediumPhrases) signs
    , playMakeBelieve = Maybe.map (EverySet.member PlayMakeBelieve) signs
    , followThreeStepInstructions = Maybe.map (EverySet.member FollowThreeStepInstructions) signs
    , standOnOneFootFiveSeconds = Maybe.map (EverySet.member StandOnOneFootFiveSeconds) signs
    , useLongPhrases = Maybe.map (EverySet.member UseLongPhrases) signs
    , shareWithOtherChildren = Maybe.map (EverySet.member ShareWithOtherChildren) signs
    , countToTen = Maybe.map (EverySet.member CountToTen) signs
    }


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


dangerSignsTasksCompletedFromTotal : WellChildMeasurements -> DangerSignsData -> DangerSignsTask -> ( Int, Int )
dangerSignsTasksCompletedFromTotal measurements data task =
    case task of
        TaskSymptomsReview ->
            let
                form =
                    measurements.symptomsReview
                        |> getMeasurementValueFunc
                        |> symptomsReviewFormWithDefault data.symptomsReviewForm
            in
            ( taskCompleted form.symptoms
            , 1
            )

        TaskVitals ->
            let
                form =
                    measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
            in
            ( taskCompleted form.respiratoryRate + taskCompleted form.bodyTemperature
            , 2
            )


immunisationTaskCompleted : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> Pages.WellChildActivity.Model.ImmunisationTask -> Bool
immunisationTaskCompleted currentDate isChw data db task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectImmunisationTask currentDate isChw data db
    in
    case task of
        TaskBCG ->
            (not <| taskExpected TaskBCG) || isJust measurements.bcgImmunisation

        TaskDTP ->
            (not <| taskExpected TaskDTP) || isJust measurements.dtpImmunisation

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


expectImmunisationTask : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> Pages.WellChildActivity.Model.ImmunisationTask -> Bool
expectImmunisationTask currentDate isChw assembled db task =
    if isChw then
        case task of
            TaskBCG ->
                True

            TaskOPV ->
                True

            _ ->
                False

    else
        let
            futureVaccinations =
                generateFutureVaccinationsData currentDate assembled.person False assembled.vaccinationHistory
                    |> Dict.fromList

            ageInWeeks =
                Maybe.map
                    (\birthDate ->
                        Date.diff Weeks birthDate currentDate
                    )
                    assembled.person.birthDate

            isTaskExpected vaccineType =
                Dict.get vaccineType futureVaccinations
                    |> Maybe.Extra.join
                    |> Maybe.map
                        (\( dose, date ) ->
                            let
                                defaultCondition =
                                    not <| Date.compare date currentDate == GT
                            in
                            if vaccineType == VaccineOPV then
                                case dose of
                                    VaccineDoseFirst ->
                                        Maybe.map
                                            (\ageWeeks ->
                                                -- First dose of OPV vaccine is given within first 2
                                                -- weeks from birth, or, starting from 6 weeks after birth.
                                                -- In latter case, there're only 3 doses, and not 4.
                                                if ageWeeks >= 2 && ageWeeks <= 5 then
                                                    False

                                                else
                                                    defaultCondition
                                            )
                                            ageInWeeks
                                            |> Maybe.withDefault False

                                    VaccineDoseSecond ->
                                        Maybe.map
                                            (\ageWeeks ->
                                                -- Second dose of OPV vaccine is given starting from
                                                -- 6 weeks after birth.
                                                if ageWeeks < 6 then
                                                    False

                                                else
                                                    defaultCondition
                                            )
                                            ageInWeeks
                                            |> Maybe.withDefault False

                                    _ ->
                                        defaultCondition

                            else
                                defaultCondition
                        )
                    |> Maybe.withDefault False
        in
        immunisationTaskToVaccineType task
            |> Maybe.map isTaskExpected
            -- Only task that is not converted to vaccine type
            -- is 'Overview', which we allways show.
            |> Maybe.withDefault True


immunisationVaccinationTasks : List ImmunisationTask
immunisationVaccinationTasks =
    [ TaskBCG
    , TaskOPV
    , TaskDTP
    , TaskPCV13
    , TaskRotarix
    , TaskIPV
    , TaskMR
    , TaskHPV
    ]


immunisationTasks : List ImmunisationTask
immunisationTasks =
    immunisationVaccinationTasks ++ [ TaskOverview ]


generateSuggestedVaccinations : NominalDate -> Bool -> AssembledData -> List ( VaccineType, VaccineDose )
generateSuggestedVaccinations currentDate isChw assembled =
    if isChw then
        [ ( VaccineBCG, VaccineDoseFirst ), ( VaccineOPV, VaccineDoseFirst ) ]

    else
        let
            initialOpvAdministered =
                wasInitialOpvAdministeredByVaccinationProgress assembled.person assembled.vaccinationProgress
        in
        List.filter (expectVaccineForPerson currentDate assembled.person initialOpvAdministered) allVaccineTypes
            |> List.filterMap
                (\vaccineType ->
                    let
                        suggestedDose =
                            case latestVaccinationDataForVaccine assembled.vaccinationHistory vaccineType of
                                Just ( lastDoseAdministered, lastDoseDate ) ->
                                    nextDoseForVaccine currentDate lastDoseDate initialOpvAdministered lastDoseAdministered vaccineType

                                Nothing ->
                                    Just VaccineDoseFirst
                    in
                    Maybe.map (\nextDose -> ( vaccineType, nextDose )) suggestedDose
                )


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData : NominalDate -> Person -> Bool -> VaccinationProgressDict -> List ( VaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData currentDate person scheduleFirstDoseForToday vaccinationProgress =
    let
        initialOpvAdministered =
            wasInitialOpvAdministeredByVaccinationProgress person vaccinationProgress
    in
    allVaccineTypesForPerson person
        |> List.map
            (\vaccineType ->
                let
                    nextVaccinationData =
                        case latestVaccinationDataForVaccine vaccinationProgress vaccineType of
                            Just ( lastDoseAdministered, lastDoseDate ) ->
                                nextVaccinationDataForVaccine lastDoseDate initialOpvAdministered lastDoseAdministered vaccineType

                            Nothing ->
                                -- There were no vaccination so far, so
                                -- we offer first dose for today.
                                let
                                    initialDate =
                                        Maybe.map (\birthDate -> initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, VaccineDoseFirst )) person.birthDate
                                            |> Maybe.withDefault currentDate

                                    vaccinationDate =
                                        if scheduleFirstDoseForToday then
                                            Date.max initialDate currentDate

                                        else
                                            initialDate
                                in
                                Just ( VaccineDoseFirst, vaccinationDate )
                in
                -- Getting Nothing at nextVaccinationData indicates that
                -- vacination cycle is completed for this vaccine.
                ( vaccineType, nextVaccinationData )
            )


{-| Check if the first dose of vaccine may be administered to the person on the limit date.
-}
expectVaccineForPerson : NominalDate -> Person -> Bool -> VaccineType -> Bool
expectVaccineForPerson limitDate person initialOpvAdministered vaccineType =
    expectVaccineDoseForPerson limitDate person initialOpvAdministered ( vaccineType, VaccineDoseFirst )


{-| Check if a dose of vaccine may be administered to a person on the limit date.
For example, to check if the dose of vaccine may be administered today, we set
limit date to current date. If we want to check in one year, we set the limit date
to current date + 1 year.
-}
expectVaccineDoseForPerson : NominalDate -> Person -> Bool -> ( VaccineType, VaccineDose ) -> Bool
expectVaccineDoseForPerson limitDate person initialOpvAdministered ( vaccineType, vaccineDose ) =
    person.birthDate
        |> Maybe.map
            (\birthDate ->
                let
                    expectedDate =
                        initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, vaccineDose )

                    compared =
                        Date.compare expectedDate limitDate

                    genderCondition =
                        if vaccineType == VaccineHPV then
                            person.gender == Female

                        else
                            True
                in
                (compared == LT || compared == EQ) && genderCondition
            )
        |> Maybe.withDefault False


initialVaccinationDateByBirthDate : NominalDate -> Bool -> ( VaccineType, VaccineDose ) -> NominalDate
initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, vaccineDose ) =
    let
        dosesInterval =
            vaccineDoseToComparable vaccineDose - 1

        ( interval, unit ) =
            getIntervalForVaccine vaccineType
    in
    case vaccineType of
        VaccineBCG ->
            birthDate

        VaccineOPV ->
            case vaccineDose of
                VaccineDoseFirst ->
                    birthDate

                _ ->
                    if initialOpvAdministered then
                        -- Second dose is given starting from age of 6 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit ((dosesInterval - 1) * interval)

                    else
                        -- Second dose is given starting from age of 10 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit (dosesInterval * interval)

        VaccineDTP ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccinePCV13 ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineRotarix ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineIPV ->
            Date.add Weeks 14 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineMR ->
            Date.add Weeks 36 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineHPV ->
            Date.add Years 12 birthDate
                |> Date.add unit (dosesInterval * interval)


latestVaccinationDataForVaccine : VaccinationProgressDict -> VaccineType -> Maybe ( VaccineDose, NominalDate )
latestVaccinationDataForVaccine vaccinationsData vaccineType =
    Dict.get vaccineType vaccinationsData
        |> Maybe.andThen
            (Dict.toList
                >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                >> List.reverse
                >> List.head
            )


nextVaccinationDataForVaccine : NominalDate -> Bool -> VaccineDose -> VaccineType -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine lastDoseDate initialOpvAdministered lastDoseAdministered vaccineType =
    if getLastDoseForVaccine initialOpvAdministered vaccineType == lastDoseAdministered then
        Nothing

    else
        getNextVaccineDose lastDoseAdministered
            |> Maybe.map
                (\dose ->
                    let
                        ( interval, unit ) =
                            getIntervalForVaccine vaccineType
                    in
                    ( dose, Date.add unit interval lastDoseDate )
                )


nextDoseForVaccine : NominalDate -> NominalDate -> Bool -> VaccineDose -> VaccineType -> Maybe VaccineDose
nextDoseForVaccine currentDate lastDoseDate initialOpvAdministered lastDoseAdministered vaccineType =
    nextVaccinationDataForVaccine lastDoseDate initialOpvAdministered lastDoseAdministered vaccineType
        |> Maybe.andThen
            (\( dose, dueDate ) ->
                if Date.compare dueDate currentDate == GT then
                    Nothing

                else
                    Just dose
            )


immunisationTaskToVaccineType : ImmunisationTask -> Maybe VaccineType
immunisationTaskToVaccineType task =
    case task of
        TaskBCG ->
            Just VaccineBCG

        TaskDTP ->
            Just VaccineDTP

        TaskHPV ->
            Just VaccineHPV

        TaskIPV ->
            Just VaccineIPV

        TaskMR ->
            Just VaccineMR

        TaskOPV ->
            Just VaccineOPV

        TaskPCV13 ->
            Just VaccinePCV13

        TaskRotarix ->
            Just VaccineRotarix

        TaskOverview ->
            Nothing


getFormByVaccineTypeFunc : VaccineType -> (ImmunisationData -> VaccinationForm)
getFormByVaccineTypeFunc vaccineType =
    case vaccineType of
        VaccineBCG ->
            .bcgForm

        VaccineDTP ->
            .dtpForm

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


getMeasurementByVaccineTypeFunc : VaccineType -> WellChildMeasurements -> Maybe VaccinationValue
getMeasurementByVaccineTypeFunc vaccineType measurements =
    case vaccineType of
        VaccineBCG ->
            measurements.bcgImmunisation
                |> getMeasurementValueFunc

        VaccineDTP ->
            measurements.dtpImmunisation
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


updateVaccinationFormByVaccineType : VaccineType -> VaccinationForm -> ImmunisationData -> ImmunisationData
updateVaccinationFormByVaccineType vaccineType form data =
    case vaccineType of
        VaccineBCG ->
            { data | bcgForm = form }

        VaccineDTP ->
            { data | dtpForm = form }

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


getAllDosesForVaccine : Bool -> VaccineType -> List VaccineDose
getAllDosesForVaccine initialOpvAdministered vaccineType =
    let
        lastDose =
            getLastDoseForVaccine initialOpvAdministered vaccineType
    in
    List.filterMap
        (\dose ->
            if vaccineDoseToComparable dose <= vaccineDoseToComparable lastDose then
                Just dose

            else
                Nothing
        )
        allVaccineDoses


getLastDoseForVaccine : Bool -> VaccineType -> VaccineDose
getLastDoseForVaccine initialOpvAdministered vaccineType =
    case vaccineType of
        VaccineBCG ->
            VaccineDoseFirst

        VaccineOPV ->
            if initialOpvAdministered then
                VaccineDoseFourth

            else
                VaccineDoseThird

        VaccineDTP ->
            VaccineDoseThird

        VaccinePCV13 ->
            VaccineDoseThird

        VaccineRotarix ->
            VaccineDoseSecond

        VaccineIPV ->
            VaccineDoseFirst

        VaccineMR ->
            VaccineDoseSecond

        VaccineHPV ->
            VaccineDoseSecond


getIntervalForVaccine : VaccineType -> ( Int, Unit )
getIntervalForVaccine vaccineType =
    case vaccineType of
        VaccineBCG ->
            ( 0, Days )

        VaccineOPV ->
            ( 4, Weeks )

        VaccineDTP ->
            ( 4, Weeks )

        VaccinePCV13 ->
            ( 4, Weeks )

        VaccineRotarix ->
            ( 4, Weeks )

        VaccineIPV ->
            ( 0, Days )

        VaccineMR ->
            ( 6, Months )

        VaccineHPV ->
            ( 6, Months )


allVaccineTypesForPerson : Person -> List VaccineType
allVaccineTypesForPerson person =
    List.filter
        (\vaccineType ->
            case vaccineType of
                VaccineHPV ->
                    person.gender == Female

                _ ->
                    True
        )
        allVaccineTypes


allVaccineTypes : List VaccineType
allVaccineTypes =
    [ VaccineBCG
    , VaccineOPV
    , VaccineDTP
    , VaccinePCV13
    , VaccineRotarix
    , VaccineIPV
    , VaccineMR
    , VaccineHPV
    ]


allVaccineDoses : List VaccineDose
allVaccineDoses =
    [ VaccineDoseFirst, VaccineDoseSecond, VaccineDoseThird, VaccineDoseFourth ]


getNextVaccineDose : VaccineDose -> Maybe VaccineDose
getNextVaccineDose dose =
    case dose of
        VaccineDoseFirst ->
            Just VaccineDoseSecond

        VaccineDoseSecond ->
            Just VaccineDoseThird

        VaccineDoseThird ->
            Just VaccineDoseFourth

        VaccineDoseFourth ->
            Nothing


vaccineDoseToComparable : VaccineDose -> Int
vaccineDoseToComparable dose =
    case dose of
        VaccineDoseFirst ->
            1

        VaccineDoseSecond ->
            2

        VaccineDoseThird ->
            3

        VaccineDoseFourth ->
            4


fromVaccinationValue : Maybe VaccinationValue -> VaccinationForm
fromVaccinationValue saved =
    Maybe.map
        (\value ->
            { administeredDoses = Just value.administeredDoses
            , administeredDosesDirty = True
            , administrationDates = Just value.administrationDates
            , administrationNote = Just value.administrationNote
            , administrationNoteDirty = False
            , viewMode = ViewModeInitial
            , updatePreviousVaccines = Just False
            , willReceiveVaccineToday = value.administrationNote == AdministeredToday |> Just
            , vaccinationUpdateDate = Nothing
            , dateSelectorOpen = False
            }
        )
        saved
        |> Maybe.withDefault emptyVaccinationForm


vaccinationFormWithDefault : VaccinationForm -> Maybe VaccinationValue -> VaccinationForm
vaccinationFormWithDefault form saved =
    unwrap
        form
        (\value ->
            let
                administrationNote =
                    valueConsideringIsDirtyField form.administrationNoteDirty form.administrationNote value.administrationNote
            in
            { administeredDoses = or form.administeredDoses (Just value.administeredDoses)
            , administeredDosesDirty = form.administeredDosesDirty
            , administrationDates = or form.administrationDates (Just value.administrationDates)
            , administrationNote = administrationNote
            , administrationNoteDirty = form.administrationNoteDirty
            , viewMode = form.viewMode
            , updatePreviousVaccines = or form.updatePreviousVaccines (Just False)
            , willReceiveVaccineToday = or form.willReceiveVaccineToday (administrationNote == Just AdministeredToday |> Just)
            , vaccinationUpdateDate = form.vaccinationUpdateDate
            , dateSelectorOpen = form.dateSelectorOpen
            }
        )
        saved


toVaccinationValueWithDefault : Maybe VaccinationValue -> VaccinationForm -> Maybe VaccinationValue
toVaccinationValueWithDefault saved form =
    vaccinationFormWithDefault form saved
        |> toVaccinationValue


toVaccinationValue : VaccinationForm -> Maybe VaccinationValue
toVaccinationValue form =
    let
        administeredDoses =
            Maybe.withDefault EverySet.empty form.administeredDoses

        administrationDates =
            Maybe.withDefault EverySet.empty form.administrationDates

        administrationNote =
            Maybe.withDefault AdministeredPreviously form.administrationNote
    in
    Just <| VaccinationValue administeredDoses administrationDates administrationNote


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
        |> List.map
            (\measurements ->
                measurements.ecd
                    |> Maybe.map (Tuple.second >> .value >> EverySet.toList)
                    |> Maybe.withDefault []
            )
        |> List.concat
        |> List.filter ((/=) NoECDSigns)
        -- Eliminate duplicate occurances.
        |> EverySet.fromList
        |> EverySet.toList


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
                        groupedECDSigns ageMonths assembled
                in
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
            )
        |> Maybe.withDefault []


groupedECDSigns : Int -> AssembledData -> List (List ECDSign)
groupedECDSigns ageMonths assembled =
    let
        ageMonthsAtLastAssessment =
            ageInMonthsAtLastAssessment assembled

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


medicationTaskCompleted : NominalDate -> Bool -> AssembledData -> MedicationTask -> Bool
medicationTaskCompleted currentDate isChw assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectMedicationTask currentDate isChw assembled
    in
    case task of
        TaskAlbendazole ->
            (not <| taskExpected TaskAlbendazole) || isJust measurements.albendazole

        TaskMebendezole ->
            (not <| taskExpected TaskMebendezole) || isJust measurements.mebendezole

        TaskVitaminA ->
            (not <| taskExpected TaskVitaminA) || isJust measurements.vitaminA


expectMedicationTask : NominalDate -> Bool -> AssembledData -> MedicationTask -> Bool
expectMedicationTask currentDate isChw assembled task =
    let
        nextAdmnistrationData =
            getPreviousMeasurements assembled.previousMeasurementsWithDates
                |> nextMedicationAdmnistrationData currentDate assembled.person
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


nextMedicationAdmnistrationData : NominalDate -> Person -> List WellChildMeasurements -> Dict MedicationTask NominalDate
nextMedicationAdmnistrationData currentDate person measurements =
    List.filter (expectMedicationByAge currentDate person) medicationTasks
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


expectMedicationByAge : NominalDate -> Person -> MedicationTask -> Bool
expectMedicationByAge currentDate person task =
    ageInMonths currentDate person
        |> Maybe.map
            (\ageMonths ->
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


medicationTasksCompletedFromTotal : WellChildMeasurements -> MedicationData -> MedicationTask -> ( Int, Int )
medicationTasksCompletedFromTotal measurements data task =
    let
        processMedicationAdministrationTask form =
            let
                ( nonAdministrationCompleted, nonAdministrationActive ) =
                    if form.medicationAdministered == Just False then
                        ( taskCompleted form.reasonForNonAdministration, 1 )

                    else
                        ( 0, 0 )
            in
            ( taskCompleted form.medicationAdministered + nonAdministrationCompleted
            , 1 + nonAdministrationActive
            )
    in
    case task of
        TaskAlbendazole ->
            measurements.albendazole
                |> getMeasurementValueFunc
                |> medicationAdministrationFormWithDefault data.albendazoleForm
                |> processMedicationAdministrationTask

        TaskMebendezole ->
            measurements.mebendezole
                |> getMeasurementValueFunc
                |> medicationAdministrationFormWithDefault data.mebendezoleForm
                |> processMedicationAdministrationTask

        TaskVitaminA ->
            measurements.vitaminA
                |> getMeasurementValueFunc
                |> medicationAdministrationFormWithDefault data.vitaminAForm
                |> processMedicationAdministrationTask


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


resolveAlbendazoleDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveAlbendazoleDosageAndIcon currentDate person =
    Just ( "400 mg", "icon-pills" )


resolveMebendezoleDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveMebendezoleDosageAndIcon currentDate person =
    Just ( "500 mg", "icon-pills" )


resolveVitaminADosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveVitaminADosageAndIcon currentDate person =
    ageInMonths currentDate person
        |> Maybe.map
            (\ageMonths ->
                if ageMonths < 18 then
                    ( "100,000 IU", "icon-capsule blue" )

                else
                    ( "200,000 IU", "icon-capsule red" )
            )


nextStepsTaskCompleted : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> Pages.WellChildActivity.Model.NextStepsTask -> Bool
nextStepsTaskCompleted currentDate zscores isChw data db task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectNextStepsTask currentDate zscores isChw data db
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


expectNextStepsTask : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> Pages.WellChildActivity.Model.NextStepsTask -> Bool
expectNextStepsTask currentDate zscores isChw assembled db task =
    case task of
        TaskContributingFactors ->
            if mandatoryNutritionAssessmentTasksCompleted currentDate isChw assembled db then
                -- Any assesment require Next Steps tasks.
                generateNutritionAssessment currentDate zscores db assembled
                    |> List.isEmpty
                    |> not

            else
                False

        TaskHealthEducation ->
            expectNextStepsTask currentDate zscores isChw assembled db TaskContributingFactors
                || -- At newborn exam, CHW should provide Health Education,
                   -- if newborn was not vaccinated at birth.
                   (isChw
                        && activityCompleted currentDate zscores isChw assembled db WellChildImmunisation
                        && (not <| newbornVaccinatedAtBirth assembled.measurements)
                   )

        TaskFollowUp ->
            expectNextStepsTask currentDate zscores isChw assembled db TaskContributingFactors

        TaskSendToHC ->
            expectNextStepsTask currentDate zscores isChw assembled db TaskContributingFactors
                || -- At newborn exam, CHW should send patient to HC,
                   -- if newborn was not vaccinated at birth.
                   (isChw
                        && activityCompleted currentDate zscores isChw assembled db WellChildImmunisation
                        && (not <| newbornVaccinatedAtBirth assembled.measurements)
                   )

        TaskNextVisit ->
            not isChw
                -- Activity that triggers Nutrition Assessment next steps is completed.
                && activityCompleted currentDate zscores isChw assembled db WellChildNutritionAssessment
                -- Activities that affect determinating next visit date are
                -- either completed, or not shown at current visit.
                && activityCompleted currentDate zscores isChw assembled db WellChildImmunisation
                && activityCompleted currentDate zscores isChw assembled db WellChildECD
                && activityCompleted currentDate zscores isChw assembled db WellChildMedication
                && nextVisitRequired currentDate isChw assembled db


newbornVaccinatedAtBirth : WellChildMeasurements -> Bool
newbornVaccinatedAtBirth measurements =
    List.all ((==) (Just AdministeredToday))
        [ getMeasurementValueFunc measurements.bcgImmunisation
            |> Maybe.map .administrationNote
        , getMeasurementValueFunc measurements.opvImmunisation
            |> Maybe.map .administrationNote
        ]


nextStepsTasksCompletedFromTotal : Bool -> WellChildMeasurements -> NextStepsData -> Pages.WellChildActivity.Model.NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal isChw measurements data task =
    case task of
        TaskContributingFactors ->
            let
                form =
                    measurements.contributingFactors
                        |> getMeasurementValueFunc
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
            in
            ( taskCompleted form.signs
            , 1
            )

        TaskHealthEducation ->
            let
                form =
                    measurements.healthEducation
                        |> getMeasurementValueFunc
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

        TaskFollowUp ->
            let
                form =
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )

        TaskSendToHC ->
            let
                form =
                    measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
            in
            if isChw then
                let
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

            else
                ( taskCompleted form.enrollToNutritionProgram + taskCompleted form.referToNutritionProgram
                , 2
                )

        TaskNextVisit ->
            let
                form =
                    measurements.nextVisit
                        |> getMeasurementValueFunc
                        |> nextVisitFormWithDefault data.nextVisitForm
            in
            ( taskAnyCompleted [ form.immunisationDate, form.pediatricVisitDate ]
            , 1
            )


nextStepsTasks : List Pages.WellChildActivity.Model.NextStepsTask
nextStepsTasks =
    [ TaskContributingFactors, TaskHealthEducation, TaskSendToHC, TaskFollowUp, TaskNextVisit ]


nextVisitRequired : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> Bool
nextVisitRequired currentDate isChw assembled db =
    let
        ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
            generateNextVisitDates currentDate isChw assembled db
    in
    isJust nextDateForImmunisationVisit || isJust nextDateForPediatricVisit


generateNextVisitDates : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> ( Maybe NominalDate, Maybe NominalDate )
generateNextVisitDates currentDate isChw assembled db =
    let
        nextVisitDateForECD =
            generateNextDateForECDVisit currentDate assembled db

        nextVisitDateForMedication =
            generateNextDateForMedicationVisit currentDate assembled db
    in
    ( generateNextDateForImmunisationVisit currentDate isChw assembled db
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

                    ageMonths =
                        Date.diff Months birthDate currentDate

                    ageYears =
                        Date.diff Years birthDate currentDate

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

                else if ageMonths < 6 then
                    Just <| Date.add Months 6 birthDate

                else if ageMonths < 15 then
                    Just <| Date.add Months 15 birthDate

                else if ageYears < 2 then
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


generateNextDateForMedicationVisit : NominalDate -> AssembledData -> ModelIndexedDb -> Maybe NominalDate
generateNextDateForMedicationVisit currentDate assembled db =
    assembled.person.birthDate
        |> Maybe.andThen
            (\birthDate ->
                let
                    ageMonths =
                        Date.diff Months birthDate currentDate
                in
                -- When younder than 6 months, set visit date to
                -- age of 6 months.
                if ageMonths < 6 then
                    Just <| Date.add Months 6 birthDate

                else
                    let
                        measurements =
                            assembled.measurements :: getPreviousMeasurements assembled.previousMeasurementsWithDates

                        nextDate =
                            nextMedicationAdmnistrationData currentDate assembled.person measurements
                                |> Dict.values
                                |> List.sortWith Date.compare
                                |> List.reverse
                                |> List.head
                    in
                    Maybe.andThen
                        (\date ->
                            let
                                compared =
                                    Date.compare date currentDate
                            in
                            if compared == LT || compared == EQ then
                                -- Next date already passed, or, it's due today.
                                -- Per requirements, we schedule next date as if medication
                                -- was administered today.
                                Just <| Date.add Months 6 currentDate

                            else
                                Just date
                        )
                        nextDate
            )


generateNextDateForImmunisationVisit : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> Maybe NominalDate
generateNextDateForImmunisationVisit currentDate isChw assembled db =
    let
        futureVaccinationsData =
            generateFutureVaccinationsData currentDate assembled.person True assembled.vaccinationProgress

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
    Maybe.andThen
        (\nextDate ->
            if Date.compare nextDate currentDate /= GT then
                Just <| Date.add unit interval currentDate

            else
                Just nextDate
        )
        nextVisitDate


fromNextVisitValue : Maybe NextVisitValue -> NextVisitForm
fromNextVisitValue saved =
    { immunisationDate = Maybe.andThen .immunisationDate saved
    , pediatricVisitDate = Maybe.andThen .pediatricVisitDate saved
    }


nextVisitFormWithDefault : NextVisitForm -> Maybe NextVisitValue -> NextVisitForm
nextVisitFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { immunisationDate = or form.immunisationDate value.immunisationDate
                , pediatricVisitDate = or form.pediatricVisitDate value.pediatricVisitDate
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
            form.pediatricVisitDate


generateVaccinationProgress : Person -> List WellChildMeasurements -> VaccinationProgressDict
generateVaccinationProgress person measurements =
    let
        bcgImmunisations =
            List.filterMap (.bcgImmunisation >> getMeasurementValueFunc)
                measurements

        dtpImmunisations =
            List.filterMap (.dtpImmunisation >> getMeasurementValueFunc)
                measurements

        ipvImmunisations =
            List.filterMap (.ipvImmunisation >> getMeasurementValueFunc)
                measurements

        mrImmunisations =
            List.filterMap (.mrImmunisation >> getMeasurementValueFunc)
                measurements

        opvImmunisations =
            List.filterMap (.opvImmunisation >> getMeasurementValueFunc)
                measurements

        pcv13Immunisations =
            List.filterMap (.pcv13Immunisation >> getMeasurementValueFunc)
                measurements

        rotarixImmunisations =
            List.filterMap (.rotarixImmunisation >> getMeasurementValueFunc)
                measurements

        hpvProgress =
            if person.gender == Female then
                let
                    hpvImmunisations =
                        List.filterMap (.hpvImmunisation >> getMeasurementValueFunc)
                            measurements
                in
                [ ( VaccineHPV, generateVaccinationProgressForVaccine hpvImmunisations ) ]

            else
                []
    in
    [ ( VaccineBCG, generateVaccinationProgressForVaccine bcgImmunisations )
    , ( VaccineOPV, generateVaccinationProgressForVaccine opvImmunisations )
    , ( VaccineDTP, generateVaccinationProgressForVaccine dtpImmunisations )
    , ( VaccinePCV13, generateVaccinationProgressForVaccine pcv13Immunisations )
    , ( VaccineRotarix, generateVaccinationProgressForVaccine rotarixImmunisations )
    , ( VaccineIPV, generateVaccinationProgressForVaccine ipvImmunisations )
    , ( VaccineMR, generateVaccinationProgressForVaccine mrImmunisations )
    ]
        ++ hpvProgress
        |> Dict.fromList


generateVaccinationProgressForVaccine : List VaccinationValue -> Dict VaccineDose NominalDate
generateVaccinationProgressForVaccine vaccinations =
    List.foldl
        (\vaccination accum ->
            let
                doses =
                    EverySet.toList vaccination.administeredDoses
                        |> List.sortBy vaccineDoseToComparable

                dates =
                    EverySet.toList vaccination.administrationDates
                        |> List.sortWith Date.compare
            in
            accum ++ List.Extra.zip doses dates
        )
        []
        vaccinations
        |> List.sortBy (Tuple.first >> vaccineDoseToComparable)
        |> Dict.fromList


wasInitialOpvAdministeredByVaccinationProgress : Person -> VaccinationProgressDict -> Bool
wasInitialOpvAdministeredByVaccinationProgress person vaccinationProgress =
    let
        firstDoseAdminstrationDate =
            Dict.get VaccineOPV vaccinationProgress
                |> Maybe.andThen (Dict.get VaccineDoseFirst)
    in
    Maybe.map2
        (\adminstrationDate birthDate ->
            Date.diff Days birthDate adminstrationDate < 14
        )
        firstDoseAdminstrationDate
        person.birthDate
        |> Maybe.withDefault False


wasInitialOpvAdministeredByVaccinationForm : NominalDate -> VaccinationForm -> Bool
wasInitialOpvAdministeredByVaccinationForm birthDate form =
    Maybe.map2
        (\administeredDoses administrationDates ->
            if EverySet.member VaccineDoseFirst administeredDoses then
                let
                    firstDoseAdminstrationDate =
                        EverySet.toList administrationDates
                            |> List.sortWith Date.compare
                            |> List.head
                in
                Maybe.map
                    (\adminstrationDate ->
                        Date.diff Days birthDate adminstrationDate < 14
                    )
                    firstDoseAdminstrationDate
                    |> Maybe.withDefault False

            else
                False
        )
        form.administeredDoses
        form.administrationDates
        |> Maybe.withDefault False



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


getPreviousMeasurements : List ( NominalDate, ( is, a ) ) -> List a
getPreviousMeasurements =
    List.map (Tuple.second >> Tuple.second)
