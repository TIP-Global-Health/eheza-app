module Pages.WellChildActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolveAllWeightMeasurementsForChild)
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
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils
    exposing
        ( taskCompleted
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


view : Language -> NominalDate -> WellChildEncounterId -> WellChildActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity data


viewHeaderAndContent : Language -> NominalDate -> WellChildEncounterId -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate id activity db model data
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


viewContent : Language -> NominalDate -> WellChildEncounterId -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id activity db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate id activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> WellChildEncounterId -> WellChildActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate id activity assembled db model =
    case activity of
        WellChildECD ->
            viewECDContent language currentDate assembled model.ecdForm

        _ ->
            []


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

        expected =
            expectedEcdSignsByAge ageMonth
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
