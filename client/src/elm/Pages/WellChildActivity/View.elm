module Pages.WellChildActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
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
import Pages.NutritionActivity.View exposing (viewHeightForm, viewMuacForm, viewNutritionForm, viewPhotoForm, viewWeightForm)
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
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreLengthHeightForAge, zScoreWeightForHeight, zScoreWeightForLength)


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw activity db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id isChw activity db model) identity data


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id isChw activity db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores id isChw activity db model data
    in
    div [ class "page-activity well-child" ]
        [ header
        , content
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


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw activity db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate zscores id isChw activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate zscores id isChw activity assembled db model =
    case activity of
        WellChildNutritionAssessment ->
            viewNutritionAssessmenContent language currentDate zscores id isChw assembled db model.nutritionAssessmentData

        WellChildECD ->
            viewECDContent language currentDate assembled model.ecdForm


viewNutritionAssessmenContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> WellChildEncounterId
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> NutritionAssessmentData
    -> List (Html Msg)
viewNutritionAssessmenContent language currentDate zscores id isChw assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            allNutritionAssesmentTasks
                |> List.filter (expectNutritionAssessmentTask currentDate zscores isChw assembled db)

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

                -- @todo:
                _ ->
                    []

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            saveMsg =
                                case task of
                                    TaskContributingFactors ->
                                        SaveContributingFactors personId measurements.contributingFactors nextTask

                                    TaskHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    TaskFollowUp ->
                                        let
                                            assesment =
                                                -- @todo:
                                                -- generateNutritionAssesment currentDate zscores db assembled
                                                --     |> nutritionAssesmentForBackend
                                                EverySet.empty
                                        in
                                        SaveFollowUp personId measurements.followUp assesment nextTask

                                    TaskSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

                                    -- @todo
                                    _ ->
                                        SaveSendToHC personId measurements.sendToHC nextTask
                        in
                        viewAction language saveMsg (tasksCompleted /= totalTasks)
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui three column grid" ] <|
            List.map viewTask tasks
        ]
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
            (\ageMonth ->
                let
                    totalTasks =
                        List.length tasks

                    tasksCompleted =
                        List.map taskCompleted tasks
                            |> List.sum

                    ( inputs, tasks ) =
                        ecdFormInputsAndTasks language currentDate assembled ageMonth ecdForm

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
ecdFormInputsAndTasks language currentDate assembled ageMonth ecdForm =
    let
        form =
            assembled.measurements.ecd
                |> Maybe.map (Tuple.second >> .value)
                |> wellChildECDFormWithDefault ecdForm

        age0to8Section =
            let
                respontToSoundWithSoundUpdateFunc value form_ =
                    { form_ | respontToSoundWithSound = Just value }

                turnHeadWhenCalledUpdateFunc value form_ =
                    { form_ | turnHeadWhenCalled = Just value }

                sitWithoutSupportUpdateFunc value form_ =
                    { form_ | sitWithoutSupport = Just value }

                smileBackUpdateFunc value form_ =
                    { form_ | smileBack = Just value }

                rollTummyToBackUpdateFunc value form_ =
                    { form_ | rollTummyToBack = Just value }

                reachForToysUpdateFunc value form_ =
                    { form_ | reachForToys = Just value }
            in
            ( viewECDInput RespontToSoundWithSound form.respontToSoundWithSound respontToSoundWithSoundUpdateFunc
                ++ viewECDInput TurnHeadWhenCalled form.turnHeadWhenCalled turnHeadWhenCalledUpdateFunc
                ++ viewECDInput SitWithoutSupport form.sitWithoutSupport sitWithoutSupportUpdateFunc
                ++ viewECDInput SmileBack form.smileBack smileBackUpdateFunc
                ++ viewECDInput RollTummyToBack form.rollTummyToBack rollTummyToBackUpdateFunc
                ++ viewECDInput ReachForToys form.reachForToys reachForToysUpdateFunc
            , [ form.respontToSoundWithSound
              , form.turnHeadWhenCalled
              , form.sitWithoutSupport
              , form.smileBack
              , form.rollTummyToBack
              , form.reachForToys
              ]
            )

        age9to14Section =
            let
                useSimpleGesturesUpdateFunc value form_ =
                    { form_ | useSimpleGestures = Just value }

                standOnTheirOwnUpdateFunc value form_ =
                    { form_ | standOnTheirOwn = Just value }

                copyDuringPlayUpdateFunc value form_ =
                    { form_ | copyDuringPlay = Just value }

                sayMamaDadaUpdateFunc value form_ =
                    { form_ | sayMamaDada = Just value }

                canHoldSmallObjectsUpdateFunc value form_ =
                    { form_ | canHoldSmallObjects = Just value }
            in
            ( viewECDInput UseSimpleGestures form.useSimpleGestures useSimpleGesturesUpdateFunc
                ++ viewECDInput StandOnTheirOwn form.standOnTheirOwn standOnTheirOwnUpdateFunc
                ++ viewECDInput CopyDuringPlay form.copyDuringPlay copyDuringPlayUpdateFunc
                ++ viewECDInput SayMamaDada form.sayMamaDada sayMamaDadaUpdateFunc
                ++ viewECDInput CanHoldSmallObjects form.canHoldSmallObjects canHoldSmallObjectsUpdateFunc
            , [ form.useSimpleGestures
              , form.standOnTheirOwn
              , form.copyDuringPlay
              , form.sayMamaDada
              , form.canHoldSmallObjects
              ]
            )

        age15to17Section =
            let
                looksWhenPointedAtUpdateFunc value form_ =
                    { form_ | looksWhenPointedAt = Just value }

                useSingleWordsUpdateFunc value form_ =
                    { form_ | useSingleWords = Just value }

                walkWithoutHelpUpdateFunc value form_ =
                    { form_ | walkWithoutHelp = Just value }

                playPretendUpdateFunc value form_ =
                    { form_ | playPretend = Just value }

                pointToThingsOfInterestUpdateFunc value form_ =
                    { form_ | pointToThingsOfInterest = Just value }
            in
            ( viewECDInput LooksWhenPointedAt form.looksWhenPointedAt looksWhenPointedAtUpdateFunc
                ++ viewECDInput UseSingleWords form.useSingleWords useSingleWordsUpdateFunc
                ++ viewECDInput WalkWithoutHelp form.walkWithoutHelp walkWithoutHelpUpdateFunc
                ++ viewECDInput PlayPretend form.playPretend playPretendUpdateFunc
                ++ viewECDInput PointToThingsOfInterest form.pointToThingsOfInterest pointToThingsOfInterestUpdateFunc
            , [ form.looksWhenPointedAt
              , form.useSingleWords
              , form.walkWithoutHelp
              , form.playPretend
              , form.pointToThingsOfInterest
              ]
            )

        age18to23Section =
            let
                useShortPhrasesUpdateFunc value form_ =
                    { form_ | useShortPhrases = Just value }

                interestedInOtherChildrenUpdateFunc value form_ =
                    { form_ | interestedInOtherChildren = Just value }

                followSimlpeInstructionsUpdateFunc value form_ =
                    { form_ | followSimlpeInstructions = Just value }

                kickBallUpdateFunc value form_ =
                    { form_ | kickBall = Just value }

                pointAtNamedObjectsUpdateFunc value form_ =
                    { form_ | pointAtNamedObjects = Just value }
            in
            ( viewECDInput UseShortPhrases form.useShortPhrases useShortPhrasesUpdateFunc
                ++ viewECDInput InterestedInOtherChildren form.interestedInOtherChildren interestedInOtherChildrenUpdateFunc
                ++ viewECDInput FollowSimpleInstructions form.followSimlpeInstructions followSimlpeInstructionsUpdateFunc
                ++ viewECDInput KickBall form.kickBall kickBallUpdateFunc
                ++ viewECDInput PointAtNamedObjects form.pointAtNamedObjects pointAtNamedObjectsUpdateFunc
            , [ form.useShortPhrases
              , form.interestedInOtherChildren
              , form.followSimlpeInstructions
              , form.kickBall
              , form.pointAtNamedObjects
              ]
            )

        age24to35Section =
            let
                dressThemselvesUpdateFunc value form_ =
                    { form_ | dressThemselves = Just value }

                washHandsGoToToiledUpdateFunc value form_ =
                    { form_ | washHandsGoToToiled = Just value }

                knowsColorsAndNumbersUpdateFunc value form_ =
                    { form_ | knowsColorsAndNumbers = Just value }

                useMediumPhrasesUpdateFunc value form_ =
                    { form_ | useMediumPhrases = Just value }

                playMakeBelieveUpdateFunc value form_ =
                    { form_ | playMakeBelieve = Just value }
            in
            ( viewECDInput DressThemselves form.dressThemselves dressThemselvesUpdateFunc
                ++ viewECDInput WashHandsGoToToiled form.washHandsGoToToiled washHandsGoToToiledUpdateFunc
                ++ viewECDInput KnowsColorsAndNumbers form.knowsColorsAndNumbers knowsColorsAndNumbersUpdateFunc
                ++ viewECDInput UseMediumPhrases form.useMediumPhrases useMediumPhrasesUpdateFunc
                ++ viewECDInput PlayMakeBelieve form.playMakeBelieve playMakeBelieveUpdateFunc
            , [ form.dressThemselves
              , form.washHandsGoToToiled
              , form.knowsColorsAndNumbers
              , form.useMediumPhrases
              , form.playMakeBelieve
              ]
            )

        age36to47Section =
            let
                followThreeStepInstructionsUpdateFunc value form_ =
                    { form_ | followThreeStepInstructions = Just value }

                standOnOneFootFiveSecondsUpdateFunc value form_ =
                    { form_ | standOnOneFootFiveSeconds = Just value }

                useLongPhrasesUpdateFunc value form_ =
                    { form_ | useLongPhrases = Just value }

                shareWithOtherChildrenUpdateFunc value form_ =
                    { form_ | shareWithOtherChildren = Just value }

                countToTenUpdateFunc value form_ =
                    { form_ | countToTen = Just value }
            in
            ( viewECDInput FollowThreeStepInstructions form.followThreeStepInstructions followThreeStepInstructionsUpdateFunc
                ++ viewECDInput StandOnOneFootFiveSeconds form.standOnOneFootFiveSeconds standOnOneFootFiveSecondsUpdateFunc
                ++ viewECDInput UseLongPhrases form.useLongPhrases useLongPhrasesUpdateFunc
                ++ viewECDInput ShareWithOtherChildren form.shareWithOtherChildren shareWithOtherChildrenUpdateFunc
                ++ viewECDInput CountToTen form.countToTen countToTenUpdateFunc
            , [ form.followThreeStepInstructions
              , form.standOnOneFootFiveSeconds
              , form.useLongPhrases
              , form.shareWithOtherChildren
              , form.countToTen
              ]
            )

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
    if ageMonth < 9 then
        age0to8Section

    else if ageMonth < 15 then
        age9to14Section

    else if ageMonth < 18 then
        age15to17Section

    else if ageMonth < 24 then
        age18to23Section

    else if ageMonth < 36 then
        age24to35Section

    else
        age36to47Section


viewAction : Language -> Msg -> Bool -> Html Msg
viewAction language saveMsg disabled =
    div [ class "actions" ]
        [ button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
            , onClick saveMsg
            ]
            [ text <| translate language Translate.Save ]
        ]
