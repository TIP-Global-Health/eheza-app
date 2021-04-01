module Pages.HomeVisitActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolveAllWeightMeasurementsForChild)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (..)
import Pages.HomeVisitActivity.Model exposing (..)
import Pages.HomeVisitActivity.Utils exposing (..)
import Pages.HomeVisitEncounter.Model exposing (AssembledData)
import Pages.HomeVisitEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils
    exposing
        ( taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity data


viewHeaderAndContent : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate id activity db model data
    in
    div [ class "page-activity home-visit" ]
        [ header
        , content
        ]


viewHeader : Language -> HomeVisitEncounterId -> HomeVisitActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.HomeVisitActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| HomeVisitEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id activity db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate id activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate id activity assembled db model =
    case activity of
        Feeding ->
            viewFeedingContent language currentDate assembled db model.feedingForm

        _ ->
            []


viewFeedingContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionFeedingForm -> List (Html Msg)
viewFeedingContent language currentDate assembled db feedingForm =
    let
        form =
            assembled.measurements.feeding
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionFeedingFormWithDefault feedingForm

        totalTasks =
            10 + supplementTypeActive

        tasksCompleted =
            taskCompleted form.receiveSupplement
                + supplementTypeCompleted
                + taskCompleted form.rationPresentAtHome
                + taskCompleted form.enoughTillNextSession
                + taskCompleted form.supplementShared
                + taskCompleted form.sachetsPerDay
                + taskCompleted form.encouragedToEat
                + taskCompleted form.refusingToEat
                + taskCompleted form.breastfeeding
                + taskCompleted form.cleanWaterAvailable
                + taskCompleted form.eatenWithWater

        ( supplementTypeCompleted, supplementTypeActive ) =
            if form.receiveSupplement == Just True then
                ( taskCompleted form.supplementType, 1 )

            else
                ( 0, 0 )

        disabled =
            tasksCompleted /= totalTasks

        receiveSupplementUpdateFunc value form_ =
            { form_ | receiveSupplement = Just value, supplementType = Nothing }

        receiveSupplementSection =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion ReceiveSupplement
            , viewBoolInput language
                form.receiveSupplement
                (SetFeedingBoolInput receiveSupplementUpdateFunc)
                "receive-supplement"
                Nothing
            ]
                ++ derivedQuestion

        derivedQuestion =
            if form.receiveSupplement == Just True then
                [ viewQuestionLabel language Translate.WhatType
                , viewCheckBoxSelectInput language
                    [ FortifiedPorridge, Rutf, Ongera, TherapeutikMilk ]
                    []
                    form.supplementType
                    SetNutritionSupplementType
                    Translate.NutritionSupplementType
                ]

            else
                []

        rationPresentAtHomeUpdateFunc value form_ =
            { form_ | rationPresentAtHome = Just value }

        rationPresentAtHomeInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion RationPresentAtHome
            , viewBoolInput language
                form.rationPresentAtHome
                (SetFeedingBoolInput rationPresentAtHomeUpdateFunc)
                "ration-present-at-home"
                Nothing
            ]

        enoughTillNextSessionUpdateFunc value form_ =
            { form_ | enoughTillNextSession = Just value }

        enoughTillNextSessionInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EnoughTillNextSession
            , viewBoolInput language
                form.enoughTillNextSession
                (SetFeedingBoolInput enoughTillNextSessionUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        supplementSharedUpdateFunc value form_ =
            { form_ | supplementShared = Just value }

        supplementSharedInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion SupplementShared
            , viewBoolInput language
                form.supplementShared
                (SetFeedingBoolInput supplementSharedUpdateFunc)
                "enough-till-next-section"
                (Just ( Translate.Shared, Translate.OnlySickChild ))
            ]

        encouragedToEatUpdateFunc value form_ =
            { form_ | encouragedToEat = Just value }

        encouragedToEatInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EncouragedToEat
            , viewBoolInput language
                form.encouragedToEat
                (SetFeedingBoolInput encouragedToEatUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        refusingToEatUpdateFunc value form_ =
            { form_ | refusingToEat = Just value }

        refusingToEatInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion RefusingToEat
            , viewBoolInput language
                form.refusingToEat
                (SetFeedingBoolInput refusingToEatUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        breastfeedingUpdateFunc value form_ =
            { form_ | breastfeeding = Just value }

        breastfeedingInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion FeedingSignBreastfeeding
            , viewBoolInput language
                form.breastfeeding
                (SetFeedingBoolInput breastfeedingUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        cleanWaterAvailableUpdateFunc value form_ =
            { form_ | cleanWaterAvailable = Just value }

        cleanWaterAvailableInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion CleanWaterAvailable
            , viewBoolInput language
                form.cleanWaterAvailable
                (SetFeedingBoolInput cleanWaterAvailableUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        eatenWithWaterUpdateFunc value form_ =
            { form_ | eatenWithWater = Just value }

        eatenWithWaterInput =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EatenWithWater
            , viewBoolInput language
                form.eatenWithWater
                (SetFeedingBoolInput eatenWithWaterUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        sachetsPerDayInput =
            [ viewQuestionLabel language Translate.SachetsPerDayQuestion
            , sachetsPerDayHelper
            , option
                [ value ""
                , selected (form.sachetsPerDay == Nothing)
                ]
                [ text "" ]
                :: (List.repeat 20 0.5
                        |> List.indexedMap
                            (\index number ->
                                let
                                    s =
                                        String.fromFloat (toFloat index * number)
                                in
                                option
                                    [ value s
                                    , selected (form.sachetsPerDay == Just (toFloat index * number))
                                    ]
                                    [ text s ]
                            )
                   )
                |> select [ onInput SetSachetsPerDay, class "form-input select" ]
            ]

        allWeightMeasuements =
            resolveAllWeightMeasurementsForChild assembled.participant.person db

        childWeight =
            List.head allWeightMeasuements
                |> Maybe.map Tuple.second

        sachetsPerDayHelper =
            childWeight
                |> Maybe.map
                    (\weight ->
                        let
                            recommendation =
                                if weight < 4 then
                                    1.5

                                else if weight < 5.5 then
                                    2

                                else if weight < 7 then
                                    2.5

                                else if weight < 8.5 then
                                    3

                                else if weight < 9.5 then
                                    3.5

                                else if weight < 10.5 then
                                    4

                                else if weight < 12 then
                                    4.5

                                else
                                    5
                        in
                        viewCustomLabel language (Translate.SachetsPerDayHelper weight recommendation) "." "helper"
                    )
                |> Maybe.withDefault emptyNode

        content =
            receiveSupplementSection
                ++ rationPresentAtHomeInput
                ++ enoughTillNextSessionInput
                ++ supplementSharedInput
                ++ sachetsPerDayInput
                ++ encouragedToEatInput
                ++ refusingToEatInput
                ++ breastfeedingInput
                ++ cleanWaterAvailableInput
                ++ eatenWithWaterInput
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            content
                ++ viewAction language (SaveFeeding assembled.participant.person assembled.measurements.feeding) disabled
        ]
    ]


viewAction : Language -> Msg -> Bool -> List (Html Msg)
viewAction language saveMsg disabled =
    [ div [ class "actions" ]
        [ button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
            , onClick saveMsg
            ]
            [ text <| translate language Translate.Save ]
        ]
    ]
