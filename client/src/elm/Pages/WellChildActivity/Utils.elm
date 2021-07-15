module Pages.WellChildActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (WellChildEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (headCircumferenceValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (..)
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue, ifTrue, taskCompleted, valueConsideringIsDirtyField)
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..))
import ZScore.Model exposing (Kilograms(..))
import ZScore.Utils exposing (zScoreWeightForAge)


generateNutritionAssesment : NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> List NutritionAssesment
generateNutritionAssesment currentDate zscores db assembled =
    let
        measurements =
            assembled.measurements

        muacValue =
            Maybe.map (Tuple.second >> .value) measurements.muac

        nutritionValue =
            Maybe.map (Tuple.second >> .value) measurements.nutrition

        weightValue =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> weightValueFunc
                )
                measurements.weight
    in
    Backend.NutritionEncounter.Utils.generateNutritionAssesment currentDate zscores assembled.participant.person muacValue nutritionValue weightValue False db


activityCompleted : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
activityCompleted currentDate zscores isChw data db activity =
    let
        measurements =
            data.measurements

        activityExpected =
            expectActivity currentDate data db
    in
    case activity of
        WellChildDangerSigns ->
            (not <| activityExpected WellChildDangerSigns)
                || (isJust measurements.symptomsReview && isJust measurements.vitals)

        WellChildNutritionAssessment ->
            let
                ( mandatory, optional ) =
                    partitionNutritionAssessmentTasks isChw
            in
            if mandatoryNutritionAssesmentTasksCompleted currentDate zscores isChw data db then
                let
                    nonEmptyAssessment =
                        generateNutritionAssesment currentDate zscores db data
                            |> List.isEmpty
                            |> not
                in
                if nonEmptyAssessment then
                    List.all (nutritionAssessmentTaskCompleted currentDate zscores isChw data db) (optional ++ nutritionAssessmentNextStepsTasks)

                else
                    List.all (nutritionAssessmentTaskCompleted currentDate zscores isChw data db) optional

            else
                False

        WellChildECD ->
            (not <| activityExpected WellChildECD) || isJust measurements.ecd

        WellChildMedication ->
            (not <| activityExpected WellChildMedication)
                || (isJust measurements.mebendezole && isJust measurements.vitaminA)


expectActivity : NominalDate -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
expectActivity currentDate assembled db activity =
    case activity of
        WellChildECD ->
            ageInMonths currentDate assembled.person
                |> Maybe.map
                    (\ageMonths ->
                        let
                            completed =
                                generateCompletedECDSigns assembled
                        in
                        expectedECDSignsByAge ageMonths
                            |> List.filter (\sign -> not <| List.member sign completed)
                            |> List.isEmpty
                            |> not
                    )
                |> Maybe.withDefault False

        WellChildMedication ->
            -- @todo
            True

        _ ->
            True


nutritionAssessmentTaskCompleted : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> NutritionAssesmentTask -> Bool
nutritionAssessmentTaskCompleted currentDate zscores isChw data db task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectNutritionAssessmentTask currentDate zscores isChw data db
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

        TaskContributingFactors ->
            (not <| taskExpected TaskContributingFactors) || isJust measurements.contributingFactors

        TaskHealthEducation ->
            (not <| taskExpected TaskHealthEducation) || isJust measurements.healthEducation

        TaskFollowUp ->
            (not <| taskExpected TaskFollowUp) || isJust measurements.followUp

        TaskSendToHC ->
            (not <| taskExpected TaskContributingFactors) || isJust measurements.sendToHC


expectNutritionAssessmentTask : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> NutritionAssesmentTask -> Bool
expectNutritionAssessmentTask currentDate zscores isChw data db task =
    case task of
        -- Show for children that are at least 6 month old.
        TaskMuac ->
            ageInMonths currentDate data.person
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        TaskContributingFactors ->
            if mandatoryNutritionAssesmentTasksCompleted currentDate zscores isChw data db then
                -- Any assesment require Next Steps tasks.
                generateNutritionAssesment currentDate zscores db data
                    |> List.isEmpty
                    |> not

            else
                False

        TaskHealthEducation ->
            expectNutritionAssessmentTask currentDate zscores isChw data db TaskContributingFactors

        TaskFollowUp ->
            expectNutritionAssessmentTask currentDate zscores isChw data db TaskContributingFactors

        TaskSendToHC ->
            expectNutritionAssessmentTask currentDate zscores isChw data db TaskContributingFactors

        -- View any other task.
        _ ->
            True


mandatoryNutritionAssesmentTasksCompleted : NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> ModelIndexedDb -> Bool
mandatoryNutritionAssesmentTasksCompleted currentDate zscores isChw data db =
    partitionNutritionAssessmentTasks isChw
        |> Tuple.first
        |> List.filter (not << nutritionAssessmentTaskCompleted currentDate zscores isChw data db)
        |> List.isEmpty


{-| List of activities that need to be completed, in order to
decide if to show Next Steps activity, or not.
-}
partitionNutritionAssessmentTasks : Bool -> ( List NutritionAssesmentTask, List NutritionAssesmentTask )
partitionNutritionAssessmentTasks isChw =
    if isChw then
        ( [ TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ], [ TaskHeight, TaskPhoto ] )

    else
        ( [ TaskHeight, TaskHeadCircumference, TaskMuac, TaskNutrition, TaskWeight ], [ TaskPhoto ] )


nutritionAssessmentNextStepsTasks : List NutritionAssesmentTask
nutritionAssessmentNextStepsTasks =
    [ TaskContributingFactors, TaskHealthEducation, TaskFollowUp, TaskSendToHC ]


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
    { respontToSoundWithSound = Maybe.map (EverySet.member RespontToSoundWithSound) signs
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
                { respontToSoundWithSound = or form.respontToSoundWithSound (EverySet.member RespontToSoundWithSound signs |> Just)
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
    [ ifNullableTrue RespontToSoundWithSound form.respontToSoundWithSound
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
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsReviewFormWithDefault data.symptomsReviewForm
            in
            ( taskCompleted form.symptoms
            , 1
            )

        TaskVitals ->
            let
                form =
                    measurements.vitals
                        |> Maybe.map (Tuple.second >> .value)
                        |> basicVitalsFormWithDefault data.vitalsForm
            in
            ( taskCompleted form.respiratoryRate + taskCompleted form.bodyTemperature
            , 2
            )


nutritionAssessmentTasksCompletedFromTotal : WellChildMeasurements -> NutritionAssessmentData -> NutritionAssesmentTask -> ( Int, Int )
nutritionAssessmentTasksCompletedFromTotal measurements data task =
    case task of
        TaskHeight ->
            let
                form =
                    measurements.height
                        |> Maybe.map (Tuple.second >> .value)
                        |> heightFormWithDefault data.heightForm
            in
            ( taskCompleted form.height
            , 1
            )

        TaskHeadCircumference ->
            let
                form =
                    measurements.headCircumference
                        |> Maybe.map (Tuple.second >> .value)
                        |> headCircumferenceFormWithDefault data.headCircumferenceForm
            in
            ( taskCompleted form.headCircumference
            , 1
            )

        TaskMuac ->
            let
                form =
                    measurements.muac
                        |> Maybe.map (Tuple.second >> .value)
                        |> muacFormWithDefault data.muacForm
            in
            ( taskCompleted form.muac
            , 1
            )

        TaskNutrition ->
            let
                form =
                    measurements.nutrition
                        |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
                        |> weightFormWithDefault data.weightForm
            in
            ( taskCompleted form.weight
            , 1
            )

        TaskContributingFactors ->
            let
                form =
                    measurements.contributingFactors
                        |> Maybe.map (Tuple.second >> .value)
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
            in
            ( taskCompleted form.signs
            , 1
            )

        TaskHealthEducation ->
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

        TaskFollowUp ->
            let
                form =
                    measurements.followUp
                        |> Maybe.map (Tuple.second >> .value)
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )

        TaskSendToHC ->
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


generateCompletedECDSigns : AssembledData -> List ECDSign
generateCompletedECDSigns assembled =
    assembled.previousMeasurementsWithDates
        |> List.map
            (\( _, ( _, measurements ) ) ->
                measurements.ecd
                    |> Maybe.map (Tuple.second >> .value >> EverySet.toList)
                    |> Maybe.withDefault []
            )
        |> List.concat
        |> List.filter ((/=) NoECDSigns)
        -- Eliminate duplicate occurances.
        |> EverySet.fromList
        |> EverySet.toList


expectedECDSignsByAge : Int -> List ECDSign
expectedECDSignsByAge ageMonths =
    if ageMonths < 6 then
        []

    else if ageMonths < 9 then
        List.Extra.splitAt 1 groupedECDSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 15 then
        List.Extra.splitAt 2 groupedECDSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 18 then
        List.Extra.splitAt 3 groupedECDSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 24 then
        List.Extra.splitAt 4 groupedECDSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 36 then
        List.Extra.splitAt 5 groupedECDSigns
            |> Tuple.first
            |> List.concat

    else
        List.concat groupedECDSigns


groupedECDSigns : List (List ECDSign)
groupedECDSigns =
    [ ecdSigns6to8
    , ecdSigns9to14
    , ecdSigns15to17
    , ecdSigns18to23
    , ecdSigns24to35
    , ecdSigns36to47
    ]


ecdSigns6to8 : List ECDSign
ecdSigns6to8 =
    [ RespontToSoundWithSound
    , TurnHeadWhenCalled
    , SitWithoutSupport
    , SmileBack
    , RollTummyToBack
    , ReachForToys
    ]


ecdSigns9to14 : List ECDSign
ecdSigns9to14 =
    [ UseSimpleGestures
    , StandOnTheirOwn
    , CopyDuringPlay
    , SayMamaDada
    , CanHoldSmallObjects
    ]


ecdSigns15to17 : List ECDSign
ecdSigns15to17 =
    [ LooksWhenPointedAt
    , UseSingleWords
    , WalkWithoutHelp
    , PlayPretend
    , PointToThingsOfInterest
    ]


ecdSigns18to23 : List ECDSign
ecdSigns18to23 =
    [ UseShortPhrases
    , InterestedInOtherChildren
    , FollowSimpleInstructions
    , KickBall
    , PointAtNamedObjects
    ]


ecdSigns24to35 : List ECDSign
ecdSigns24to35 =
    [ DressThemselves
    , WashHandsGoToToiled
    , KnowsColorsAndNumbers
    , UseMediumPhrases
    , PlayMakeBelieve
    ]


ecdSigns36to47 : List ECDSign
ecdSigns36to47 =
    [ FollowThreeStepInstructions
    , StandOnOneFootFiveSeconds
    , UseLongPhrases
    , ShareWithOtherChildren
    , CountToTen
    ]


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
                |> Maybe.map (Tuple.second >> .value)
                |> medicationAdministrationFormWithDefault data.albendazoleForm
                |> processMedicationAdministrationTask

        TaskMebendezole ->
            measurements.mebendezole
                |> Maybe.map (Tuple.second >> .value)
                |> medicationAdministrationFormWithDefault data.mebendezoleForm
                |> processMedicationAdministrationTask

        TaskVitaminA ->
            measurements.vitaminA
                |> Maybe.map (Tuple.second >> .value)
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
