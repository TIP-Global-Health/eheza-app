module Pages.WellChildActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (nutritionAssesmentForBackend)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( renderDatePart
        , viewContributingFactorsForm
        , viewFollowUpForm
        , viewHealthEducationForm
        , viewMeasurementFloatDiff
        , viewMuacIndication
        , viewSendToHCForm
        , zScoreForHeightOrLength
        )
import Pages.NutritionActivity.View exposing (viewHeightForm, viewMuacForm, viewNutritionForm, viewPhotoForm, viewWeightForm, warningPopup)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , taskCompletedWithException
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewQuestionLabel
        )
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildActivity.Utils exposing (..)
import Pages.WellChildEncounter.Model exposing (AssembledData)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreLengthHeightForAge, zScoreWeightForHeight, zScoreWeightForLength)


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> WellChildActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id activity db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id activity db model) identity data


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id activity db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores id activity db model data
    in
    div [ class "page-activity well-child" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                currentDate
                SetWarningPopupState
                model.warningPopupState
        ]


viewHeader : Language -> WellChildEncounterId -> WellChildActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.WellChildActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| WellChildEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id activity db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate zscores id activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> WellChildActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate zscores id activity assembled db model =
    case activity of
        WellChildNutritionAssessment ->
            viewNutritionAssessmenContent language currentDate zscores id assembled db model.nutritionAssessmentData

        WellChildECD ->
            viewECDContent language currentDate assembled model.ecdForm


viewNutritionAssessmenContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> WellChildEncounterId
    -> AssembledData
    -> ModelIndexedDb
    -> NutritionAssessmentData
    -> List (Html Msg)
viewNutritionAssessmenContent language currentDate zscores id assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            allNutritionAssesmentTasks
                |> List.filter (expectNutritionAssessmentTask currentDate zscores assembled db)

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskHeight ->
                            ( "height"
                            , isJust measurements.height
                            )

                        TaskMuac ->
                            ( "muac"
                            , isJust measurements.muac
                            )

                        TaskNutrition ->
                            ( "nutrition"
                            , isJust measurements.nutrition
                            )

                        TaskPhoto ->
                            ( "photo"
                            , isJust measurements.photo
                            )

                        TaskWeight ->
                            ( "weight"
                            , isJust measurements.weight
                            )

                        TaskContributingFactors ->
                            ( "next-steps-contributing-factors"
                            , isJust measurements.contributingFactors
                            )

                        TaskHealthEducation ->
                            ( "next-steps-health-education"
                            , isJust measurements.healthEducation
                            )

                        TaskFollowUp ->
                            ( "next-steps-follow-up"
                            , isJust measurements.followUp
                            )

                        TaskSendToHC ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.sendToHC
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveNutritionAssesmentTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NutritionAssesmentTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map (\task -> ( task, nutritionAssessmentTasksCompletedFromTotal measurements data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        childMeasurements =
            Dict.get assembled.participant.person db.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe

        resolvePreviousGroupValue getChildMeasurementFunc =
            childMeasurements
                |> Maybe.andThen
                    (getChildMeasurementFunc
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, measurement.value ))
                        -- Most recent date to least recent date.
                        >> List.sortWith (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))
                        >> List.head
                    )

        viewForm =
            case activeTask of
                Just TaskHeight ->
                    let
                        previousIndividualValue =
                            resolveIndividualWellChildValue assembled.previousMeasurementsWithDates .height (\(HeightInCm cm) -> cm)

                        previousGroupValue =
                            resolvePreviousGroupValue .heights
                                |> Maybe.map (\( date, HeightInCm val ) -> ( date, val ))
                    in
                    measurements.height
                        |> Maybe.map (Tuple.second >> .value)
                        |> heightFormWithDefault data.heightForm
                        |> viewHeightForm language currentDate zscores assembled.person previousGroupValue previousIndividualValue SetHeight

                Just TaskMuac ->
                    let
                        previousIndividualValue =
                            resolveIndividualWellChildValue assembled.previousMeasurementsWithDates .muac (\(MuacInCm cm) -> cm)

                        previousGroupValue =
                            resolvePreviousGroupValue .muacs
                                |> Maybe.map (\( date, MuacInCm val ) -> ( date, val ))
                    in
                    measurements.muac
                        |> Maybe.map (Tuple.second >> .value)
                        |> muacFormWithDefault data.muacForm
                        |> viewMuacForm language currentDate assembled.person previousGroupValue previousIndividualValue SetMuac

                Just TaskNutrition ->
                    measurements.nutrition
                        |> Maybe.map (Tuple.second >> .value)
                        |> nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate SetNutritionSign

                Just TaskPhoto ->
                    let
                        displayPhoto =
                            case data.photoForm.url of
                                Just url ->
                                    Just url

                                Nothing ->
                                    Maybe.map (Tuple.second >> .value) assembled.measurements.photo
                    in
                    viewPhotoForm language currentDate displayPhoto DropZoneComplete

                Just TaskWeight ->
                    let
                        heightValue =
                            assembled.measurements.height
                                |> Maybe.map (Tuple.second >> .value)

                        previousIndividualValue =
                            resolveIndividualWellChildValue assembled.previousMeasurementsWithDates .weight (\(WeightInKg cm) -> cm)

                        previousGroupValue =
                            resolvePreviousGroupValue .weights
                                |> Maybe.map (\( date, WeightInKg val ) -> ( date, val ))
                    in
                    measurements.weight
                        |> Maybe.map (Tuple.second >> .value)
                        |> weightFormWithDefault data.weightForm
                        |> viewWeightForm language currentDate zscores assembled.person heightValue previousGroupValue previousIndividualValue SetWeight

                Just TaskContributingFactors ->
                    measurements.contributingFactors
                        |> Maybe.map (Tuple.second >> .value)
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> viewContributingFactorsForm language currentDate SetContributingFactorsSign
                        |> List.singleton

                Just TaskHealthEducation ->
                    measurements.healthEducation
                        |> Maybe.map (Tuple.second >> .value)
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation
                        |> List.singleton

                Just TaskFollowUp ->
                    measurements.followUp
                        |> Maybe.map (Tuple.second >> .value)
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate SetFollowUpOption
                        |> List.singleton

                Just TaskSendToHC ->
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHCForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNotSendingToHC
                            SetHandReferralForm
                            Nothing
                        |> List.singleton

                Nothing ->
                    []

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        tasksTray =
            let
                ( topTasks, bottomTasks ) =
                    List.Extra.splitAt 5 tasks

                topSection =
                    List.map viewTask topTasks
                        |> div [ class "ui five column grid" ]

                bottomSection =
                    if List.isEmpty bottomTasks then
                        emptyNode

                    else
                        List.map viewTask bottomTasks
                            |> div [ class "ui four column grid" ]
            in
            div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
                [ topSection, bottomSection ]

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            saveMsg =
                                case task of
                                    TaskHeight ->
                                        SaveHeight personId measurements.height nextTask

                                    TaskMuac ->
                                        SaveMuac personId measurements.muac nextTask

                                    TaskNutrition ->
                                        SaveNutrition personId measurements.nutrition nextTask

                                    TaskPhoto ->
                                        let
                                            photoId =
                                                Maybe.map Tuple.first measurements.photo
                                        in
                                        data.photoForm.url
                                            |> Maybe.map (\url -> SavePhoto personId photoId url nextTask)
                                            |> Maybe.withDefault NoOp

                                    TaskWeight ->
                                        SaveWeight personId measurements.weight nextTask

                                    TaskContributingFactors ->
                                        SaveContributingFactors personId measurements.contributingFactors nextTask

                                    TaskHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    TaskFollowUp ->
                                        let
                                            assesment =
                                                generateNutritionAssesment currentDate zscores db assembled
                                                    |> nutritionAssesmentForBackend
                                        in
                                        SaveFollowUp personId measurements.followUp assesment nextTask

                                    TaskSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

                            disabled =
                                case task of
                                    TaskPhoto ->
                                        isNothing data.photoForm.url

                                    _ ->
                                        tasksCompleted /= totalTasks
                        in
                        viewAction language saveMsg disabled
                    )
                |> Maybe.withDefault emptyNode
    in
    [ tasksTray
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewECDContent : Language -> NominalDate -> AssembledData -> WellChildECDForm -> List (Html Msg)
viewECDContent language currentDate assembled ecdForm =
    ageInMonths currentDate assembled.person
        |> Maybe.map
            (\ageMonths ->
                let
                    totalTasks =
                        List.length tasks

                    tasksCompleted =
                        List.map taskCompleted tasks
                            |> List.sum

                    ( inputs, tasks ) =
                        ecdFormInputsAndTasks language currentDate assembled ageMonths ecdForm

                    disabled =
                        tasksCompleted /= totalTasks
                in
                [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
                , div [ class "ui full segment" ]
                    [ div [ class "full content" ]
                        [ div [ class "ui form ecd" ]
                            inputs
                        ]
                    , viewAction language (SaveECD assembled.participant.person assembled.measurements.ecd) disabled
                    ]
                ]
            )
        |> Maybe.withDefault []


ecdFormInputsAndTasks : Language -> NominalDate -> AssembledData -> Int -> WellChildECDForm -> ( List (Html Msg), List (Maybe Bool) )
ecdFormInputsAndTasks language currentDate assembled ageMonths ecdForm =
    let
        form =
            assembled.measurements.ecd
                |> Maybe.map (Tuple.second >> .value)
                |> wellChildECDFormWithDefault ecdForm

        completed =
            generateCompletedECDSigns assembled

        expected =
            expectedECDSignsByAge ageMonths
                |> List.filter (\sign -> not <| List.member sign completed)
                |> List.map inputAndTaskForSign

        inputAndTaskForSign sign =
            case sign of
                RespontToSoundWithSound ->
                    let
                        respontToSoundWithSoundUpdateFunc value form_ =
                            { form_ | respontToSoundWithSound = Just value }
                    in
                    ( viewECDInput RespontToSoundWithSound form.respontToSoundWithSound respontToSoundWithSoundUpdateFunc
                    , form.respontToSoundWithSound
                    )

                TurnHeadWhenCalled ->
                    let
                        turnHeadWhenCalledUpdateFunc value form_ =
                            { form_ | turnHeadWhenCalled = Just value }
                    in
                    ( viewECDInput TurnHeadWhenCalled form.turnHeadWhenCalled turnHeadWhenCalledUpdateFunc
                    , form.turnHeadWhenCalled
                    )

                SitWithoutSupport ->
                    let
                        sitWithoutSupportUpdateFunc value form_ =
                            { form_ | sitWithoutSupport = Just value }
                    in
                    ( viewECDInput SitWithoutSupport form.sitWithoutSupport sitWithoutSupportUpdateFunc
                    , form.sitWithoutSupport
                    )

                SmileBack ->
                    let
                        smileBackUpdateFunc value form_ =
                            { form_ | smileBack = Just value }
                    in
                    ( viewECDInput SmileBack form.smileBack smileBackUpdateFunc
                    , form.smileBack
                    )

                RollTummyToBack ->
                    let
                        rollTummyToBackUpdateFunc value form_ =
                            { form_ | rollTummyToBack = Just value }
                    in
                    ( viewECDInput RollTummyToBack form.rollTummyToBack rollTummyToBackUpdateFunc
                    , form.rollTummyToBack
                    )

                ReachForToys ->
                    let
                        reachForToysUpdateFunc value form_ =
                            { form_ | reachForToys = Just value }
                    in
                    ( viewECDInput ReachForToys form.reachForToys reachForToysUpdateFunc
                    , form.reachForToys
                    )

                UseSimpleGestures ->
                    let
                        useSimpleGesturesUpdateFunc value form_ =
                            { form_ | useSimpleGestures = Just value }
                    in
                    ( viewECDInput UseSimpleGestures form.useSimpleGestures useSimpleGesturesUpdateFunc
                    , form.useSimpleGestures
                    )

                StandOnTheirOwn ->
                    let
                        standOnTheirOwnUpdateFunc value form_ =
                            { form_ | standOnTheirOwn = Just value }
                    in
                    ( viewECDInput StandOnTheirOwn form.standOnTheirOwn standOnTheirOwnUpdateFunc
                    , form.standOnTheirOwn
                    )

                CopyDuringPlay ->
                    let
                        copyDuringPlayUpdateFunc value form_ =
                            { form_ | copyDuringPlay = Just value }
                    in
                    ( viewECDInput CopyDuringPlay form.copyDuringPlay copyDuringPlayUpdateFunc
                    , form.copyDuringPlay
                    )

                SayMamaDada ->
                    let
                        sayMamaDadaUpdateFunc value form_ =
                            { form_ | sayMamaDada = Just value }
                    in
                    ( viewECDInput SayMamaDada form.sayMamaDada sayMamaDadaUpdateFunc
                    , form.sayMamaDada
                    )

                CanHoldSmallObjects ->
                    let
                        canHoldSmallObjectsUpdateFunc value form_ =
                            { form_ | canHoldSmallObjects = Just value }
                    in
                    ( viewECDInput CanHoldSmallObjects form.canHoldSmallObjects canHoldSmallObjectsUpdateFunc
                    , form.canHoldSmallObjects
                    )

                LooksWhenPointedAt ->
                    let
                        looksWhenPointedAtUpdateFunc value form_ =
                            { form_ | looksWhenPointedAt = Just value }
                    in
                    ( viewECDInput LooksWhenPointedAt form.looksWhenPointedAt looksWhenPointedAtUpdateFunc
                    , form.looksWhenPointedAt
                    )

                UseSingleWords ->
                    let
                        useSingleWordsUpdateFunc value form_ =
                            { form_ | useSingleWords = Just value }
                    in
                    ( viewECDInput UseSingleWords form.useSingleWords useSingleWordsUpdateFunc
                    , form.useSingleWords
                    )

                WalkWithoutHelp ->
                    let
                        walkWithoutHelpUpdateFunc value form_ =
                            { form_ | walkWithoutHelp = Just value }
                    in
                    ( viewECDInput WalkWithoutHelp form.walkWithoutHelp walkWithoutHelpUpdateFunc
                    , form.walkWithoutHelp
                    )

                PlayPretend ->
                    let
                        playPretendUpdateFunc value form_ =
                            { form_ | playPretend = Just value }
                    in
                    ( viewECDInput PlayPretend form.playPretend playPretendUpdateFunc
                    , form.playPretend
                    )

                PointToThingsOfInterest ->
                    let
                        pointToThingsOfInterestUpdateFunc value form_ =
                            { form_ | pointToThingsOfInterest = Just value }
                    in
                    ( viewECDInput PointToThingsOfInterest form.pointToThingsOfInterest pointToThingsOfInterestUpdateFunc
                    , form.pointToThingsOfInterest
                    )

                UseShortPhrases ->
                    let
                        useShortPhrasesUpdateFunc value form_ =
                            { form_ | useShortPhrases = Just value }
                    in
                    ( viewECDInput UseShortPhrases form.useShortPhrases useShortPhrasesUpdateFunc
                    , form.useShortPhrases
                    )

                InterestedInOtherChildren ->
                    let
                        interestedInOtherChildrenUpdateFunc value form_ =
                            { form_ | interestedInOtherChildren = Just value }
                    in
                    ( viewECDInput InterestedInOtherChildren form.interestedInOtherChildren interestedInOtherChildrenUpdateFunc
                    , form.interestedInOtherChildren
                    )

                FollowSimpleInstructions ->
                    let
                        followSimlpeInstructionsUpdateFunc value form_ =
                            { form_ | followSimlpeInstructions = Just value }
                    in
                    ( viewECDInput FollowSimpleInstructions form.followSimlpeInstructions followSimlpeInstructionsUpdateFunc
                    , form.followSimlpeInstructions
                    )

                KickBall ->
                    let
                        kickBallUpdateFunc value form_ =
                            { form_ | kickBall = Just value }
                    in
                    ( viewECDInput KickBall form.kickBall kickBallUpdateFunc
                    , form.kickBall
                    )

                PointAtNamedObjects ->
                    let
                        pointAtNamedObjectsUpdateFunc value form_ =
                            { form_ | pointAtNamedObjects = Just value }
                    in
                    ( viewECDInput PointAtNamedObjects form.pointAtNamedObjects pointAtNamedObjectsUpdateFunc
                    , form.pointAtNamedObjects
                    )

                DressThemselves ->
                    let
                        dressThemselvesUpdateFunc value form_ =
                            { form_ | dressThemselves = Just value }
                    in
                    ( viewECDInput DressThemselves form.dressThemselves dressThemselvesUpdateFunc
                    , form.dressThemselves
                    )

                WashHandsGoToToiled ->
                    let
                        washHandsGoToToiledUpdateFunc value form_ =
                            { form_ | washHandsGoToToiled = Just value }
                    in
                    ( viewECDInput WashHandsGoToToiled form.washHandsGoToToiled washHandsGoToToiledUpdateFunc
                    , form.washHandsGoToToiled
                    )

                KnowsColorsAndNumbers ->
                    let
                        knowsColorsAndNumbersUpdateFunc value form_ =
                            { form_ | knowsColorsAndNumbers = Just value }
                    in
                    ( viewECDInput KnowsColorsAndNumbers form.knowsColorsAndNumbers knowsColorsAndNumbersUpdateFunc
                    , form.knowsColorsAndNumbers
                    )

                UseMediumPhrases ->
                    let
                        useMediumPhrasesUpdateFunc value form_ =
                            { form_ | useMediumPhrases = Just value }
                    in
                    ( viewECDInput UseMediumPhrases form.useMediumPhrases useMediumPhrasesUpdateFunc
                    , form.useMediumPhrases
                    )

                PlayMakeBelieve ->
                    let
                        playMakeBelieveUpdateFunc value form_ =
                            { form_ | playMakeBelieve = Just value }
                    in
                    ( viewECDInput PlayMakeBelieve form.playMakeBelieve playMakeBelieveUpdateFunc
                    , form.playMakeBelieve
                    )

                FollowThreeStepInstructions ->
                    let
                        followThreeStepInstructionsUpdateFunc value form_ =
                            { form_ | followThreeStepInstructions = Just value }
                    in
                    ( viewECDInput FollowThreeStepInstructions form.followThreeStepInstructions followThreeStepInstructionsUpdateFunc
                    , form.followThreeStepInstructions
                    )

                StandOnOneFootFiveSeconds ->
                    let
                        standOnOneFootFiveSecondsUpdateFunc value form_ =
                            { form_ | standOnOneFootFiveSeconds = Just value }
                    in
                    ( viewECDInput StandOnOneFootFiveSeconds form.standOnOneFootFiveSeconds standOnOneFootFiveSecondsUpdateFunc
                    , form.standOnOneFootFiveSeconds
                    )

                UseLongPhrases ->
                    let
                        useLongPhrasesUpdateFunc value form_ =
                            { form_ | useLongPhrases = Just value }
                    in
                    ( viewECDInput UseLongPhrases form.useLongPhrases useLongPhrasesUpdateFunc
                    , form.useLongPhrases
                    )

                ShareWithOtherChildren ->
                    let
                        shareWithOtherChildrenUpdateFunc value form_ =
                            { form_ | shareWithOtherChildren = Just value }
                    in
                    ( viewECDInput ShareWithOtherChildren form.shareWithOtherChildren shareWithOtherChildrenUpdateFunc
                    , form.shareWithOtherChildren
                    )

                CountToTen ->
                    let
                        countToTenUpdateFunc value form_ =
                            { form_ | countToTen = Just value }
                    in
                    ( viewECDInput CountToTen form.countToTen countToTenUpdateFunc
                    , form.countToTen
                    )

                NoECDSigns ->
                    ( [], Nothing )

        viewECDInput sign value updateFunc =
            [ viewQuestionLabel language <| Translate.ECDSignQuestion sign
            , viewBoolInput
                language
                value
                (SetECDBoolInput updateFunc)
                ""
                Nothing
            ]
    in
    ( List.map Tuple.first expected |> List.concat
    , List.map Tuple.second expected
    )


viewAction : Language -> Msg -> Bool -> Html Msg
viewAction language saveMsg disabled =
    div [ class "actions" ]
        [ button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
            , onClick saveMsg
            ]
            [ text <| translate language Translate.Save ]
        ]
