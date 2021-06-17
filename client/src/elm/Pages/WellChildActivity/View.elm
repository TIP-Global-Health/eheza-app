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
                ++ viewECDInput FollowSimlpeInstructions form.followSimlpeInstructions followSimlpeInstructionsUpdateFunc
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
