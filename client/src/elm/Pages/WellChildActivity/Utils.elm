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
import Pages.WellChildEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..))
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
            let
                ( mandatory, optional ) =
                    partitionNutritionAssessmentTasks isChw
            in
            (mandatory ++ optional)
                |> List.all (nutritionAssessmentTaskCompleted currentDate isChw assembled db)

        WellChildECD ->
            (not <| activityExpected WellChildECD) || isJust measurements.ecd

        WellChildMedication ->
            (not <| activityExpected WellChildMedication)
                || (isJust measurements.mebendezole && isJust measurements.vitaminA)

        WellChildVaccinationHistory ->
            (not <| activityExpected WellChildVaccinationHistory) || isJust measurements.vaccinationHistory

        WellChildImmunisation ->
            (not <| activityExpected WellChildImmunisation) || isJust measurements.immunisation

        WellChildNextSteps ->
            List.all (nextStepsTaskCompleted currentDate zscores isChw assembled db) nextStepsTasks


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

        WellChildVaccinationHistory ->
            not isChw

        WellChildImmunisation ->
            let
                suggestedVaccines =
                    generateSuggestedVaccines currentDate isChw assembled
            in
            (not <| List.isEmpty suggestedVaccines) && activityCompleted currentDate zscores isChw assembled db WellChildVaccinationHistory

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
                allMedicationTasks
                    |> List.filter (expectMedicationTask currentDate isChw assembled)
                    |> List.isEmpty
                    |> not

        WellChildNextSteps ->
            nextStepsTasks
                |> List.filter (expectNextStepsTask currentDate zscores isChw assembled db)
                |> List.isEmpty
                |> not


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
    , dateConcluded = Maybe.map .dateConcluded saved
    , isDateConcludedSelectorOpen = False
    , apgarsOneMinute = Maybe.map .apgarsOneMinute saved
    , apgarsFiveMinutes = Maybe.map .apgarsFiveMinutes saved
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
                , dateConcluded = or form.dateConcluded (Just value.dateConcluded)
                , isDateConcludedSelectorOpen = form.isDateConcludedSelectorOpen
                , apgarsOneMinute = or form.apgarsOneMinute (Just value.apgarsOneMinute)
                , apgarsFiveMinutes = or form.apgarsFiveMinutes (Just value.apgarsFiveMinutes)
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
        |> andMap form.dateConcluded
        |> andMap form.apgarsOneMinute
        |> andMap form.apgarsFiveMinutes
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
            (not <| taskExpected TaskHeight) || isJust measurements.headCircumference

        TaskMuac ->
            (not <| taskExpected TaskMuac) || isJust measurements.muac

        TaskNutrition ->
            (not <| taskExpected TaskNutrition) || isJust measurements.nutrition

        TaskPhoto ->
            (not <| taskExpected TaskPhoto) || isJust measurements.photo

        TaskWeight ->
            (not <| taskExpected TaskWeight) || isJust measurements.weight


expectNutritionAssessmentTask : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> NutritionAssessmentTask -> Bool
expectNutritionAssessmentTask currentDate isChw data db task =
    case task of
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
    partitionNutritionAssessmentTasks isChw
        |> Tuple.first
        |> List.filter (not << nutritionAssessmentTaskCompleted currentDate isChw data db)
        |> List.isEmpty


{-| List of activities that need to be completed, in order to
decide if to show Next Steps activity, or not.
-}
partitionNutritionAssessmentTasks : Bool -> ( List NutritionAssessmentTask, List NutritionAssessmentTask )
partitionNutritionAssessmentTasks isChw =
    if isChw then
        -- Muac is not here, because Newbor Exam is done for children that are less than 2 months old.
        ( [ TaskHeadCircumference, TaskNutrition, TaskWeight ], [ TaskPhoto ] )

    else
        ( [ TaskHeight, TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ], [ TaskPhoto ] )


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

        TaskPhoto ->
            ( if isNothing data.photoForm.url && isNothing measurements.photo then
                0

              else
                1
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
                        |> basicVitalsFormWithDefault data.vitalsForm
            in
            ( taskCompleted form.respiratoryRate + taskCompleted form.bodyTemperature
            , 2
            )


generateSuggestedVaccines : NominalDate -> Bool -> AssembledData -> List ( VaccineType, VaccineDose )
generateSuggestedVaccines currentDate isChw assembled =
    if isChw then
        [ ( VaccineBCG, VaccineDoseFirst ), ( VaccineOPV, VaccineDoseFirst ) ]

    else
        List.filter (expectVaccineForPerson currentDate assembled.person) allVaccineTypes
            |> List.filterMap
                (\vaccineType ->
                    let
                        suggestedDose =
                            case latestVaccinationDataForVaccine assembled.vaccinationHistory vaccineType of
                                Just ( lastDoseAdministered, lastDoseDate ) ->
                                    nextDoseForVaccine currentDate lastDoseDate lastDoseAdministered vaccineType

                                Nothing ->
                                    Just VaccineDoseFirst
                    in
                    Maybe.map (\nextDose -> ( vaccineType, nextDose )) suggestedDose
                )


generateFutureVaccines : NominalDate -> Bool -> AssembledData -> List ( VaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccines currentDate isChw assembled =
    List.filter (expectVaccineForPerson currentDate assembled.person) allVaccineTypes
        |> List.map
            (\vaccineType ->
                let
                    nextVaccinationData =
                        case latestVaccinationDataForVaccine assembled.vaccinationProgress vaccineType of
                            Just ( lastDoseAdministered, lastDoseDate ) ->
                                nextVaccinationDataForVaccine lastDoseDate lastDoseAdministered vaccineType

                            Nothing ->
                                -- There were no vaccination so far, so
                                -- we offer first dose for today.
                                Just ( VaccineDoseFirst, currentDate )
                in
                -- Getting Nothing at nextVaccinationData indicates that
                -- vacination cycle is completed for this vaccine.
                ( vaccineType, nextVaccinationData )
            )


expectVaccineForPerson : NominalDate -> Person -> VaccineType -> Bool
expectVaccineForPerson currentDate person vaccineType =
    expectVaccineDoseForPerson currentDate person ( vaccineType, VaccineDoseFirst )


filterExpextedDosesForPerson : NominalDate -> Person -> ( VaccineType, List VaccineDose ) -> Maybe ( VaccineType, List VaccineDose )
filterExpextedDosesForPerson currentDate person ( vaccine, doses ) =
    let
        expectedDoses =
            List.filterMap
                (\dose ->
                    if expectVaccineDoseForPerson currentDate person ( vaccine, dose ) then
                        Just dose

                    else
                        Nothing
                )
                doses
    in
    if List.isEmpty expectedDoses then
        Nothing

    else
        Just ( vaccine, expectedDoses )


expectVaccineDoseForPerson : NominalDate -> Person -> ( VaccineType, VaccineDose ) -> Bool
expectVaccineDoseForPerson currentDate person ( vaccineType, vaccineDose ) =
    person.birthDate
        |> Maybe.map
            (\birthDate ->
                let
                    doseIntervals =
                        vaccineDoseToComparable vaccineDose - 1

                    ( interval, unit ) =
                        getIntervalForVaccine vaccineType

                    compared =
                        Date.compare expectedDate currentDate

                    expectedDate =
                        case vaccineType of
                            VaccineBCG ->
                                birthDate

                            VaccineOPV ->
                                Date.add unit (doseIntervals * interval) birthDate

                            VaccineDTP ->
                                Date.add Weeks 5 birthDate
                                    |> Date.add unit (doseIntervals * interval)

                            VaccinePCV13 ->
                                Date.add Weeks 5 birthDate
                                    |> Date.add unit (doseIntervals * interval)

                            VaccineRotarix ->
                                Date.add Weeks 5 birthDate
                                    |> Date.add unit (doseIntervals * interval)

                            VaccineIPV ->
                                Date.add Weeks 13 birthDate
                                    |> Date.add unit (doseIntervals * interval)

                            VaccineMR ->
                                Date.add Weeks 35 birthDate
                                    |> Date.add unit (doseIntervals * interval)

                            VaccineHPV ->
                                Date.add Years 12 birthDate
                                    |> Date.add Weeks -1
                                    |> Date.add unit (doseIntervals * interval)

                    genderCondition =
                        if vaccineType == VaccineHPV then
                            person.gender == Female

                        else
                            True
                in
                (compared == LT || compared == EQ) && genderCondition
            )
        |> Maybe.withDefault False


latestVaccinationDataForVaccine : Dict VaccineType (Dict VaccineDose NominalDate) -> VaccineType -> Maybe ( VaccineDose, NominalDate )
latestVaccinationDataForVaccine vaccinationsData vaccineType =
    Dict.get vaccineType vaccinationsData
        |> Maybe.andThen
            (Dict.toList
                >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                >> List.reverse
                >> List.head
            )


nextVaccinationDataForVaccine : NominalDate -> VaccineDose -> VaccineType -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine lastDoseDate lastDoseAdministered vaccineType =
    if getLastDoseForVaccine vaccineType == lastDoseAdministered then
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


nextDoseForVaccine : NominalDate -> NominalDate -> VaccineDose -> VaccineType -> Maybe VaccineDose
nextDoseForVaccine currentDate lastDoseDate lastDoseAdministered vaccineType =
    nextVaccinationDataForVaccine lastDoseDate lastDoseAdministered vaccineType
        |> Maybe.andThen
            (\( dose, dueDate ) ->
                if Date.compare dueDate currentDate == GT then
                    Nothing

                else
                    Just dose
            )


getVaccinationDateFromImmunisationValue : VaccineType -> (ImmunisationValue -> Maybe NominalDate)
getVaccinationDateFromImmunisationValue vaccineType =
    case vaccineType of
        VaccineBCG ->
            .bcgVaccinationDate

        VaccineOPV ->
            .opvVaccinationDate

        VaccineDTP ->
            .dtpVaccinationDate

        VaccinePCV13 ->
            .pcv13VaccinationDate

        VaccineRotarix ->
            .rotarixVaccinationDate

        VaccineIPV ->
            .ipvVaccinationDate

        VaccineMR ->
            .mrVaccinationDate

        VaccineHPV ->
            .hpvVaccinationDate


getVaccinationDatesFromVaccinationHistoryValue : VaccineType -> (VaccinationHistoryValue -> EverySet NominalDate)
getVaccinationDatesFromVaccinationHistoryValue vaccineType =
    case vaccineType of
        VaccineBCG ->
            .bcgVaccinationDate

        VaccineOPV ->
            .opvVaccinationDate

        VaccineDTP ->
            .dtpVaccinationDate

        VaccinePCV13 ->
            .pcv13VaccinationDate

        VaccineRotarix ->
            .rotarixVaccinationDate

        VaccineIPV ->
            .ipvVaccinationDate

        VaccineMR ->
            .mrVaccinationDate

        VaccineHPV ->
            .hpvVaccinationDate


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


allVaccinesWithDoses : Dict VaccineType (List VaccineDose)
allVaccinesWithDoses =
    List.map
        (\type_ -> ( type_, getAllDosesForVaccine type_ ))
        allVaccineTypes
        |> Dict.fromList


getAllDosesForVaccine : VaccineType -> List VaccineDose
getAllDosesForVaccine vaccineType =
    let
        lastDose =
            getLastDoseForVaccine vaccineType
    in
    List.filterMap
        (\dose ->
            if vaccineDoseToComparable dose <= vaccineDoseToComparable lastDose then
                Just dose

            else
                Nothing
        )
        allVaccineDoses


getLastDoseForVaccine : VaccineType -> VaccineDose
getLastDoseForVaccine vaccineType =
    case vaccineType of
        VaccineBCG ->
            VaccineDoseFirst

        VaccineOPV ->
            VaccineDoseFourth

        VaccineDTP ->
            VaccineDoseSecond

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
            ( 28, Days )

        VaccineDTP ->
            ( 28, Days )

        VaccinePCV13 ->
            ( 28, Days )

        VaccineRotarix ->
            ( 28, Days )

        VaccineIPV ->
            ( 0, Days )

        VaccineMR ->
            ( 6, Months )

        VaccineHPV ->
            ( 6, Months )


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


fromImmunisationValue : Maybe ImmunisationValue -> ImmunisationForm
fromImmunisationValue saved =
    let
        suggestedVaccines =
            Maybe.map .suggestedVaccines saved
                |> Maybe.withDefault Dict.empty

        vacinationNotes =
            Maybe.map .vacinationNotes saved

        vaccineAdministered administrationNote =
            Maybe.map ((==) AdministeredToday) administrationNote

        resolveAdministrationNote vaccineType =
            Maybe.andThen (Dict.get vaccineType) vacinationNotes

        bcgVaccinationNote =
            resolveAdministrationNote VaccineBCG

        opvVaccinationNote =
            resolveAdministrationNote VaccineOPV

        dtpVaccinationNote =
            resolveAdministrationNote VaccineDTP

        pcv13VaccinationNote =
            resolveAdministrationNote VaccinePCV13

        rotarixVaccinationNote =
            resolveAdministrationNote VaccineRotarix

        ipvVaccinationNote =
            resolveAdministrationNote VaccineIPV

        mrVaccinationNote =
            resolveAdministrationNote VaccineMR

        hpvVaccinationNote =
            resolveAdministrationNote VaccineHPV
    in
    { suggestedVaccines = suggestedVaccines
    , bcgVaccinationAdministered = vaccineAdministered bcgVaccinationNote
    , opvVaccinationAdministered = vaccineAdministered opvVaccinationNote
    , dtpVaccinationAdministered = vaccineAdministered dtpVaccinationNote
    , pcv13VaccinationAdministered = vaccineAdministered pcv13VaccinationNote
    , rotarixVaccinationAdministered = vaccineAdministered rotarixVaccinationNote
    , ipvVaccinationAdministered = vaccineAdministered ipvVaccinationNote
    , mrVaccinationAdministered = vaccineAdministered mrVaccinationNote
    , hpvVaccinationAdministered = vaccineAdministered hpvVaccinationNote
    , bcgVaccinationNote = bcgVaccinationNote
    , opvVaccinationNote = opvVaccinationNote
    , dtpVaccinationNote = dtpVaccinationNote
    , pcv13VaccinationNote = pcv13VaccinationNote
    , rotarixVaccinationNote = rotarixVaccinationNote
    , ipvVaccinationNote = ipvVaccinationNote
    , mrVaccinationNote = mrVaccinationNote
    , hpvVaccinationNote = hpvVaccinationNote
    , bcgVaccinationDate = Maybe.andThen .bcgVaccinationDate saved
    , opvVaccinationDate = Maybe.andThen .opvVaccinationDate saved
    , dtpVaccinationDate = Maybe.andThen .dtpVaccinationDate saved
    , pcv13VaccinationDate = Maybe.andThen .pcv13VaccinationDate saved
    , rotarixVaccinationDate = Maybe.andThen .rotarixVaccinationDate saved
    , ipvVaccinationDate = Maybe.andThen .ipvVaccinationDate saved
    , mrVaccinationDate = Maybe.andThen .mrVaccinationDate saved
    , hpvVaccinationDate = Maybe.andThen .hpvVaccinationDate saved
    , bcgVaccinationDateSelectorOpen = False
    , opvVaccinationDateSelectorOpen = False
    , dtpVaccinationDateSelectorOpen = False
    , pcv13VaccinationDateSelectorOpen = False
    , rotarixVaccinationDateSelectorOpen = False
    , ipvVaccinationDateSelectorOpen = False
    , mrVaccinationDateSelectorOpen = False
    , hpvVaccinationDateSelectorOpen = False
    }


immunisationFormWithDefault : ImmunisationForm -> Maybe ImmunisationValue -> ImmunisationForm
immunisationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    vaccineAdministered administrationNote =
                        Maybe.map ((==) AdministeredToday) administrationNote

                    resolveAdministrationNote vaccineType =
                        Dict.get vaccineType value.vacinationNotes

                    bcgVaccinationNote =
                        resolveAdministrationNote VaccineBCG

                    opvVaccinationNote =
                        resolveAdministrationNote VaccineOPV

                    dtpVaccinationNote =
                        resolveAdministrationNote VaccineDTP

                    pcv13VaccinationNote =
                        resolveAdministrationNote VaccinePCV13

                    rotarixVaccinationNote =
                        resolveAdministrationNote VaccineRotarix

                    ipvVaccinationNote =
                        resolveAdministrationNote VaccineIPV

                    mrVaccinationNote =
                        resolveAdministrationNote VaccineMR

                    hpvVaccinationNote =
                        resolveAdministrationNote VaccineHPV
                in
                { suggestedVaccines = form.suggestedVaccines
                , bcgVaccinationAdministered = or form.bcgVaccinationAdministered (vaccineAdministered bcgVaccinationNote)
                , opvVaccinationAdministered = or form.opvVaccinationAdministered (vaccineAdministered opvVaccinationNote)
                , dtpVaccinationAdministered = or form.dtpVaccinationAdministered (vaccineAdministered dtpVaccinationNote)
                , pcv13VaccinationAdministered = or form.pcv13VaccinationAdministered (vaccineAdministered pcv13VaccinationNote)
                , rotarixVaccinationAdministered = or form.rotarixVaccinationAdministered (vaccineAdministered rotarixVaccinationNote)
                , ipvVaccinationAdministered = or form.ipvVaccinationAdministered (vaccineAdministered ipvVaccinationNote)
                , mrVaccinationAdministered = or form.mrVaccinationAdministered (vaccineAdministered mrVaccinationNote)
                , hpvVaccinationAdministered = or form.hpvVaccinationAdministered (vaccineAdministered hpvVaccinationNote)
                , bcgVaccinationNote = or form.bcgVaccinationNote bcgVaccinationNote
                , opvVaccinationNote = or form.opvVaccinationNote opvVaccinationNote
                , dtpVaccinationNote = or form.dtpVaccinationNote dtpVaccinationNote
                , pcv13VaccinationNote = or form.pcv13VaccinationNote pcv13VaccinationNote
                , rotarixVaccinationNote = or form.rotarixVaccinationNote rotarixVaccinationNote
                , ipvVaccinationNote = or form.ipvVaccinationNote ipvVaccinationNote
                , mrVaccinationNote = or form.mrVaccinationNote mrVaccinationNote
                , hpvVaccinationNote = or form.hpvVaccinationNote hpvVaccinationNote
                , bcgVaccinationDate = or form.bcgVaccinationDate value.bcgVaccinationDate
                , opvVaccinationDate = or form.opvVaccinationDate value.opvVaccinationDate
                , dtpVaccinationDate = or form.dtpVaccinationDate value.dtpVaccinationDate
                , pcv13VaccinationDate = or form.pcv13VaccinationDate value.pcv13VaccinationDate
                , rotarixVaccinationDate = or form.rotarixVaccinationDate value.rotarixVaccinationDate
                , ipvVaccinationDate = or form.ipvVaccinationDate value.ipvVaccinationDate
                , mrVaccinationDate = or form.mrVaccinationDate value.mrVaccinationDate
                , hpvVaccinationDate = or form.hpvVaccinationDate value.hpvVaccinationDate
                , bcgVaccinationDateSelectorOpen = form.bcgVaccinationDateSelectorOpen
                , opvVaccinationDateSelectorOpen = form.opvVaccinationDateSelectorOpen
                , dtpVaccinationDateSelectorOpen = form.dtpVaccinationDateSelectorOpen
                , pcv13VaccinationDateSelectorOpen = form.pcv13VaccinationDateSelectorOpen
                , rotarixVaccinationDateSelectorOpen = form.rotarixVaccinationDateSelectorOpen
                , ipvVaccinationDateSelectorOpen = form.ipvVaccinationDateSelectorOpen
                , mrVaccinationDateSelectorOpen = form.mrVaccinationDateSelectorOpen
                , hpvVaccinationDateSelectorOpen = form.hpvVaccinationDateSelectorOpen
                }
            )


toImmunisationValueWithDefault : Maybe ImmunisationValue -> ImmunisationForm -> Maybe ImmunisationValue
toImmunisationValueWithDefault saved form =
    immunisationFormWithDefault form saved
        |> toImmunisationValue


toImmunisationValue : ImmunisationForm -> Maybe ImmunisationValue
toImmunisationValue form =
    let
        vacinationNotes =
            List.filterMap
                (\vaccineType ->
                    determineVaccineAdministrationNote vaccineType
                        |> Maybe.map (\note -> ( vaccineType, note ))
                )
                allVaccineTypes
                |> Dict.fromList

        determineVaccineAdministrationNote vaccineType =
            case vaccineType of
                VaccineBCG ->
                    if form.bcgVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.bcgVaccinationNote

                VaccineOPV ->
                    if form.opvVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.opvVaccinationNote

                VaccineDTP ->
                    if form.dtpVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.dtpVaccinationNote

                VaccinePCV13 ->
                    if form.pcv13VaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.pcv13VaccinationNote

                VaccineRotarix ->
                    if form.rotarixVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.rotarixVaccinationNote

                VaccineIPV ->
                    if form.ipvVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.ipvVaccinationNote

                VaccineMR ->
                    if form.mrVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.mrVaccinationNote

                VaccineHPV ->
                    if form.hpvVaccinationAdministered == Just True then
                        Just AdministeredToday

                    else
                        form.hpvVaccinationNote

        determineVaccineDate getNoteFunc getDateFunc =
            let
                note =
                    getNoteFunc form
            in
            if List.member note [ Just AdministeredToday, Just AdministeredPreviously ] then
                getDateFunc form

            else
                Nothing
    in
    Just <|
        ImmunisationValue
            form.suggestedVaccines
            vacinationNotes
            (determineVaccineDate .bcgVaccinationNote .bcgVaccinationDate)
            (determineVaccineDate .opvVaccinationNote .opvVaccinationDate)
            (determineVaccineDate .dtpVaccinationNote .dtpVaccinationDate)
            (determineVaccineDate .pcv13VaccinationNote .pcv13VaccinationDate)
            (determineVaccineDate .rotarixVaccinationNote .rotarixVaccinationDate)
            (determineVaccineDate .ipvVaccinationNote .ipvVaccinationDate)
            (determineVaccineDate .mrVaccinationNote .mrVaccinationDate)
            (determineVaccineDate .hpvVaccinationNote .hpvVaccinationDate)


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
                        groupedECDSigns assembled
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


groupedECDSigns : AssembledData -> List (List ECDSign)
groupedECDSigns assembled =
    let
        sixMonthsAssessmentPerformed =
            sixMonthsECDAssessmentPerformed assembled

        ( from5Weeks, from13Weeks ) =
            if sixMonthsAssessmentPerformed then
                ( [], [] )

            else
                ( ecdSignsFrom5Weeks, ecdSignsFrom13Weeks )
    in
    [ from5Weeks
    , from13Weeks
    , ecdSignsFrom6Months sixMonthsAssessmentPerformed
    , ecdSignsFrom15Months
    , ecdSignsFrom18Months
    , ecdSignsFrom2Years
    , ecdSignsFrom3Years
    , ecdSignsFrom4Years
    ]


sixMonthsECDAssessmentPerformed : AssembledData -> Bool
sixMonthsECDAssessmentPerformed assembled =
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
                    (\assessmentDate -> Date.diff Months birthDate assessmentDate >= 6)
                    lastECDAssessmentDate
            )
        |> Maybe.withDefault False


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


ecdSignsFrom6Months : Bool -> List ECDSign
ecdSignsFrom6Months sixMonthsAssessmentPerformed =
    if sixMonthsAssessmentPerformed then
        ecdSignsFrom6MonthsMajors

    else
        ecdSignsFrom6MonthsMinors ++ ecdSignsFrom6MonthsMajors


ecdSignsFrom6MonthsMinors : List ECDSign
ecdSignsFrom6MonthsMinors =
    [ BringHandsToMouth
    , HoldHeadWithoutSupport
    , HoldAndShakeToys
    , ReactToSuddenSounds
    , UseConsonantSounds
    ]


ecdSignsFrom6MonthsMajors : List ECDSign
ecdSignsFrom6MonthsMajors =
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
    let
        administeredMebendezole =
            List.filterMap .mebendezole measurements
                |> latestAdministrationDateForMedicine

        administeredVitamineA =
            List.filterMap .mebendezole measurements
                |> latestAdministrationDateForMedicine
    in
    List.filter (expectMedicationByAge currentDate person) allMedicationTasks
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
                        List.filterMap .mebendezole measurements
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
                        ageMonths >= 60 && ageMonths < 120

                    -- 1 year to 6 years.
                    TaskMebendezole ->
                        ageMonths >= 12 && ageMonths < 60

                    -- 6 months to 6 years.
                    TaskVitaminA ->
                        ageMonths >= 6 && ageMonths < 60
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
    Just ( "500 mg", "icon-pills" )


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
                || -- At newborn exam, nurse should preovide Health Education,
                   -- if newborn was not vaccinated at birth.
                   (isChw && (newbornVaccinatedAtBirth assembled.measurements == Just False))

        TaskFollowUp ->
            expectNextStepsTask currentDate zscores isChw assembled db TaskContributingFactors

        TaskSendToHC ->
            expectNextStepsTask currentDate zscores isChw assembled db TaskContributingFactors
                || -- At newborn exam, Nurse should send patient to HC,
                   -- if newborn was not vaccinated at birth.
                   (isChw && (newbornVaccinatedAtBirth assembled.measurements == Just False))

        TaskNextVisit ->
            not isChw
                -- Activities that affect determinating next visit date are
                -- either completed, or not shown at current visit.
                && activityCompleted currentDate zscores isChw assembled db WellChildImmunisation
                && activityCompleted currentDate zscores isChw assembled db WellChildECD
                && activityCompleted currentDate zscores isChw assembled db WellChildMedication
                && nextVisitRequired currentDate isChw assembled db


newbornVaccinatedAtBirth : WellChildMeasurements -> Maybe Bool
newbornVaccinatedAtBirth measurements =
    measurements.immunisation
        |> Maybe.map
            (Tuple.second
                >> .value
                >> (\value ->
                        -- Both vaccines given at birth were administered.
                        isJust value.bcgVaccinationDate && isJust value.opvVaccinationDate
                   )
            )


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
    if List.isEmpty (generateRemianingECDSignsAfterCurrentEncounter currentDate assembled) then
        Nothing

    else
        assembled.person.birthDate
            |> Maybe.map
                (\birthDate ->
                    let
                        ageWeeks =
                            Date.diff Weeks birthDate currentDate

                        ageMonth =
                            Date.diff Months birthDate currentDate

                        ageYears =
                            Date.diff Years birthDate currentDate
                    in
                    if ageWeeks < 6 then
                        Date.add Weeks 6 birthDate

                    else if ageWeeks < 14 then
                        Date.add Weeks 14 birthDate

                    else if ageMonth < 6 then
                        Date.add Months 6 birthDate

                    else if ageMonth < 15 then
                        Date.add Months 15 birthDate

                    else if ageYears < 2 then
                        Date.add Years 2 birthDate

                    else if ageYears < 3 then
                        Date.add Years 3 birthDate

                    else if ageYears < 4 then
                        Date.add Years 4 birthDate

                    else
                        Date.add Months 6 currentDate
                )


generateNextDateForMedicationVisit : NominalDate -> AssembledData -> ModelIndexedDb -> Maybe NominalDate
generateNextDateForMedicationVisit currentDate assembled db =
    let
        measurements =
            assembled.measurements :: getPreviousMeasurements assembled.previousMeasurementsWithDates
    in
    nextMedicationAdmnistrationData currentDate assembled.person measurements
        |> Dict.values
        |> List.sortWith Date.compare
        |> List.reverse
        |> List.head


generateNextDateForImmunisationVisit : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> Maybe NominalDate
generateNextDateForImmunisationVisit currentDate isChw assembled db =
    generateFutureVaccines currentDate isChw assembled
        |> List.filterMap (Tuple.second >> Maybe.map Tuple.second)
        |> List.sortWith Date.compare
        |> List.reverse
        |> List.head


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


fromVaccinationHistoryValue : Maybe VaccinationHistoryValue -> VaccinationHistoryForm
fromVaccinationHistoryValue saved =
    let
        catchUpRequired =
            Just <| not <| Dict.isEmpty suggestedVaccinesByValue

        suggestedVaccinesByValue =
            Maybe.map .suggestedVaccines saved
                |> Maybe.withDefault Dict.empty
    in
    { catchUpRequired = catchUpRequired
    , suggestedVaccines = suggestedVaccinesByValue
    , administeredVaccines = generateAdministeredVaccinesFromValue saved
    , administeredVaccinesDirty = False
    , vaccinationDates = generateVaccinationDatesFromValue saved
    , vaccinationDatesDirty = False
    , dateSelectorsState = Dict.empty
    }


vaccinationHistoryFormWithDefault : VaccinationHistoryForm -> Maybe VaccinationHistoryValue -> VaccinationHistoryForm
vaccinationHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    catchUpRequiredByValue =
                        Just <| not <| Dict.isEmpty suggestedVaccinesByValue

                    suggestedVaccinesByValue =
                        Maybe.map .suggestedVaccines saved
                            |> Maybe.withDefault Dict.empty

                    suggestedVaccines =
                        if Dict.isEmpty form.suggestedVaccines then
                            suggestedVaccinesByValue

                        else
                            form.suggestedVaccines

                    administeredVaccines =
                        if form.administeredVaccinesDirty then
                            form.administeredVaccines

                        else
                            generateAdministeredVaccinesFromValue saved

                    vaccinationDates =
                        if form.vaccinationDatesDirty then
                            form.vaccinationDates

                        else
                            generateVaccinationDatesFromValue saved
                in
                { catchUpRequired = Maybe.Extra.or form.catchUpRequired catchUpRequiredByValue
                , suggestedVaccines = suggestedVaccines
                , administeredVaccines = administeredVaccines
                , administeredVaccinesDirty = form.administeredVaccinesDirty
                , vaccinationDates = vaccinationDates
                , vaccinationDatesDirty = form.vaccinationDatesDirty
                , dateSelectorsState = form.dateSelectorsState
                }
            )


generateAdministeredVaccinesFromValue : Maybe VaccinationHistoryValue -> Dict VaccineType (Dict VaccineDose (Maybe Bool))
generateAdministeredVaccinesFromValue saved =
    let
        everySetToDict key set =
            EverySet.toList set
                |> List.map (\dose -> ( dose, Just True ))
                |> Dict.fromList
    in
    Maybe.map (.administeredVaccines >> Dict.map everySetToDict) saved
        |> Maybe.withDefault Dict.empty


generateVaccinationDatesFromValue : Maybe VaccinationHistoryValue -> Dict VaccineType (Dict VaccineDose (Maybe NominalDate))
generateVaccinationDatesFromValue saved =
    let
        getVaccinationDateForVaccine getTypeFunc =
            Maybe.map getTypeFunc saved

        allVaccinesData =
            [ ( VaccineBCG, .bcgVaccinationDate )
            , ( VaccineOPV, .opvVaccinationDate )
            , ( VaccineDTP, .dtpVaccinationDate )
            , ( VaccinePCV13, .pcv13VaccinationDate )
            , ( VaccineRotarix, .rotarixVaccinationDate )
            , ( VaccineIPV, .ipvVaccinationDate )
            , ( VaccineMR, .mrVaccinationDate )
            , ( VaccineHPV, .hpvVaccinationDate )
            ]

        administeredVaccines =
            Maybe.map .administeredVaccines saved
                |> Maybe.withDefault Dict.empty

        datesDict =
            List.filterMap
                (\( type_, getDateFunc ) ->
                    getVaccinationDateForVaccine getDateFunc
                        |> Maybe.map
                            (\set ->
                                if EverySet.isEmpty set then
                                    Nothing

                                else
                                    Just ( type_, EverySet.toList set |> List.map Just )
                            )
                )
                allVaccinesData
                |> Maybe.Extra.values
                |> Dict.fromList
    in
    Dict.map
        (\type_ dates ->
            let
                doses =
                    Dict.get type_ administeredVaccines
                        |> Maybe.map EverySet.toList
                        |> Maybe.withDefault []
            in
            List.Extra.zip doses dates
                |> Dict.fromList
        )
        datesDict


toVaccinationHistoryValueWithDefault : Maybe VaccinationHistoryValue -> VaccinationHistoryForm -> Maybe VaccinationHistoryValue
toVaccinationHistoryValueWithDefault saved form =
    vaccinationHistoryFormWithDefault form saved
        |> toVaccinationHistoryValue


toVaccinationHistoryValue : VaccinationHistoryForm -> Maybe VaccinationHistoryValue
toVaccinationHistoryValue form =
    let
        administeredVaccines =
            Dict.map
                (\type_ doses ->
                    Dict.toList doses
                        |> List.filterMap
                            (\( dose, maybeAdministered ) ->
                                if maybeAdministered == Just True then
                                    Just dose

                                else
                                    Nothing
                            )
                        |> EverySet.fromList
                )
                form.administeredVaccines

        getVaccinationDatesForVaccine type_ =
            Dict.get type_ form.vaccinationDates
                |> Maybe.map
                    (Dict.values
                        >> List.filterMap identity
                        >> EverySet.fromList
                    )
                |> Maybe.withDefault EverySet.empty
    in
    Just <|
        { suggestedVaccines = form.suggestedVaccines
        , administeredVaccines = administeredVaccines
        , bcgVaccinationDate = getVaccinationDatesForVaccine VaccineBCG
        , opvVaccinationDate = getVaccinationDatesForVaccine VaccineOPV
        , dtpVaccinationDate = getVaccinationDatesForVaccine VaccineDTP
        , pcv13VaccinationDate = getVaccinationDatesForVaccine VaccinePCV13
        , rotarixVaccinationDate = getVaccinationDatesForVaccine VaccineRotarix
        , ipvVaccinationDate = getVaccinationDatesForVaccine VaccineIPV
        , mrVaccinationDate = getVaccinationDatesForVaccine VaccineMR
        , hpvVaccinationDate = getVaccinationDatesForVaccine VaccineHPV
        }


generateVaccinationProgress : List ImmunisationValue -> List VaccinationHistoryValue -> Dict VaccineType (Dict VaccineDose NominalDate)
generateVaccinationProgress immunisations vaccinationHistories =
    List.map (\vaccineType -> ( vaccineType, generateVaccinationProgressForVaccine immunisations vaccinationHistories vaccineType )) allVaccineTypes
        |> Dict.fromList


generateVaccinationProgressForVaccine : List ImmunisationValue -> List VaccinationHistoryValue -> VaccineType -> Dict VaccineDose NominalDate
generateVaccinationProgressForVaccine immunisations vaccinationHistories vaccineType =
    let
        dataFromImmunisation =
            generateVaccinationProgressFromImmunisationForVaccine immunisations vaccineType

        dataFromVaccinationHistory =
            generateVaccinationProgressFromVaccinationHistoryForVaccine vaccinationHistories vaccineType
    in
    Dict.union dataFromImmunisation dataFromVaccinationHistory


generateVaccinationProgressFromImmunisationForVaccine : List ImmunisationValue -> VaccineType -> Dict VaccineDose NominalDate
generateVaccinationProgressFromImmunisationForVaccine values vaccineType =
    List.filterMap
        (\value ->
            let
                suggestedDose =
                    Dict.get vaccineType value.suggestedVaccines

                vaccinationDate =
                    getVaccinationDateFromImmunisationValue vaccineType value
            in
            Maybe.map2 (\dose date -> ( dose, date ))
                suggestedDose
                vaccinationDate
        )
        values
        |> Dict.fromList


generateVaccinationProgressFromVaccinationHistoryForVaccine : List VaccinationHistoryValue -> VaccineType -> Dict VaccineDose NominalDate
generateVaccinationProgressFromVaccinationHistoryForVaccine values vaccineType =
    List.filterMap
        (\value ->
            let
                administeredDoses =
                    Dict.get vaccineType value.administeredVaccines

                vaccinationDates =
                    getVaccinationDatesFromVaccinationHistoryValue vaccineType value
            in
            Maybe.map
                (\doses ->
                    List.Extra.zip (EverySet.toList doses)
                        (EverySet.toList vaccinationDates)
                )
                administeredDoses
        )
        values
        |> List.concat
        |> Dict.fromList



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


getPreviousMeasurements : List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) ) -> List WellChildMeasurements
getPreviousMeasurements =
    List.map (Tuple.second >> Tuple.second)
