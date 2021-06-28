module Pages.HomeVisitActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolveNutritionWeightMeasurementsForChild)
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
        , taskCompletedWithException
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

        Caring ->
            viewCaringContent language currentDate assembled db model.caringForm

        Hygiene ->
            viewHygieneContent language currentDate assembled db model.hygieneForm

        FoodSecurity ->
            viewFoodSecurityContent language currentDate assembled db model.foodSecurityForm


viewFeedingContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionFeedingForm -> List (Html Msg)
viewFeedingContent language currentDate assembled db feedingForm =
    let
        form =
            assembled.measurements.feeding
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionFeedingFormWithDefault feedingForm

        totalTasks =
            5 + receiveSupplementDerivedActive + supplementTypeDerivedActive

        tasksCompleted =
            taskCompleted form.receiveSupplement
                + receiveSupplementDerivedCompleted
                + supplementTypeDerivedCompleted
                + taskCompleted form.encouragedToEat
                + taskCompleted form.refusingToEat
                + taskCompleted form.breastfeeding
                + taskCompleted form.cleanWaterAvailable

        ( receiveSupplementDerivedCompleted, receiveSupplementDerivedActive ) =
            if form.receiveSupplement == Just True then
                ( taskCompletedWithException form.supplementType NoNutritionSupplementType
                    + taskCompleted form.rationPresentAtHome
                    + taskCompleted form.enoughTillNextSession
                    + taskCompleted form.supplementShared
                , 4
                )

            else
                ( 0, 0 )

        ( supplementTypeDerivedCompleted, supplementTypeDerivedActive ) =
            if form.receiveSupplement == Just True && form.supplementType == Just Rutf then
                ( taskCompleted form.sachetsPerDay
                    + taskCompleted form.eatenWithWater
                , 2
                )

            else
                ( 0, 0 )

        disabled =
            tasksCompleted /= totalTasks

        receiveSupplementSection =
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion ReceiveSupplement
            , viewBoolInput language
                form.receiveSupplement
                (SetFeedingBoolInput receiveSupplementUpdateFunc)
                "receive-supplement"
                Nothing
            ]
                ++ receiveSupplementDerivedQuestions

        receiveSupplementDerivedQuestions =
            if form.receiveSupplement == Just True then
                [ viewQuestionLabel language Translate.WhatType
                , viewCheckBoxSelectInput language
                    [ FortifiedPorridge, Rutf, Ongera, TherapeuticMilk ]
                    []
                    form.supplementType
                    SetNutritionSupplementType
                    Translate.NutritionSupplementType
                , viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion RationPresentAtHome
                , viewBoolInput language
                    form.rationPresentAtHome
                    (SetFeedingBoolInput rationPresentAtHomeUpdateFunc)
                    "ration-present-at-home"
                    Nothing
                , viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EnoughTillNextSession
                , viewBoolInput language
                    form.enoughTillNextSession
                    (SetFeedingBoolInput enoughTillNextSessionUpdateFunc)
                    "enough-till-next-section"
                    Nothing
                , viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion SupplementShared
                , viewBoolInput language
                    form.supplementShared
                    (SetFeedingBoolInput supplementSharedUpdateFunc)
                    "enough-till-next-section"
                    (Just ( Translate.Shared, Translate.OnlySickChild ))
                ]

            else
                []

        receiveSupplementUpdateFunc value form_ =
            { form_
                | receiveSupplement = Just value
                , supplementType = Nothing
                , rationPresentAtHome = Nothing
                , enoughTillNextSession = Nothing
                , supplementShared = Nothing
            }

        rationPresentAtHomeUpdateFunc value form_ =
            { form_ | rationPresentAtHome = Just value }

        enoughTillNextSessionUpdateFunc value form_ =
            { form_ | enoughTillNextSession = Just value }

        supplementSharedUpdateFunc value form_ =
            { form_ | supplementShared = Just value }

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

        ( sachetsPerDayInput, eatenWithWaterInput ) =
            if form.receiveSupplement == Just True && form.supplementType == Just Rutf then
                ( [ viewQuestionLabel language Translate.SachetsPerDayQuestion
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
                , [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EatenWithWater
                  , viewBoolInput language
                        form.eatenWithWater
                        (SetFeedingBoolInput eatenWithWaterUpdateFunc)
                        "enough-till-next-section"
                        Nothing
                  ]
                )

            else
                ( [], [] )

        eatenWithWaterUpdateFunc value form_ =
            { form_ | eatenWithWater = Just value }

        allWeightMeasuements =
            resolveNutritionWeightMeasurementsForChild assembled.participant.person db

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


viewCaringContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionCaringForm -> List (Html Msg)
viewCaringContent language currentDate assembled db caringForm =
    let
        form =
            assembled.measurements.caring
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionCaringFormWithDefault caringForm

        totalTasks =
            3

        tasksCompleted =
            taskCompleted form.parentHealth
                + taskCompleted form.caringOption
                + taskCompleted form.childClean

        disabled =
            tasksCompleted /= totalTasks

        parentsAliveHealthyInput =
            [ viewQuestionLabel language Translate.ParentsAliveAndHealthyQuestion
            , viewBoolInput
                language
                form.parentHealth
                SetParentsAliveAndHealthy
                "parents-health"
                Nothing
            ]

        caringOptionInput =
            [ viewQuestionLabel language Translate.WhoCaresForTheChildDuringTheDay
            , viewCheckBoxSelectInput language
                [ CaredByParent
                , CaredByGrandparent
                , CaredBySibling
                , CaredByNeighbor
                , CaredByHouseHelper
                , CaredByDaycare
                ]
                []
                form.caringOption
                SetNutritionCaringOption
                Translate.NutritionCaringOption
            ]

        childCleanInput =
            [ viewQuestionLabel language Translate.ChildCleanQuestion
            , viewBoolInput
                language
                form.childClean
                SetChildClean
                "child-clean"
                Nothing
            ]

        content =
            parentsAliveHealthyInput
                ++ caringOptionInput
                ++ childCleanInput
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            content
                ++ viewAction language (SaveNutritionCaring assembled.participant.person assembled.measurements.caring) disabled
        ]
    ]


viewHygieneContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionHygieneForm -> List (Html Msg)
viewHygieneContent language currentDate assembled db hygieneForm =
    let
        form =
            assembled.measurements.hygiene
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionHygieneFormWithDefault hygieneForm

        totalTasks =
            5

        tasksCompleted =
            taskCompleted form.mainWaterSource
                + taskCompleted form.waterPreparationOption
                + taskCompleted form.soapInTheHouse
                + taskCompleted form.washHandsBeforeFeeding
                + taskCompleted form.foodCovered

        disabled =
            tasksCompleted /= totalTasks

        mainWaterSourceInput =
            [ viewQuestionLabel language Translate.MainWaterSourceQuestion
            , viewCheckBoxSelectInput language
                [ PipedWaterToHome
                , PublicWaterTap
                , RainWaterCollectionSystem
                , NaturalSourceFlowingWater
                , NaturalSourceStandingWater
                , BottledWater
                ]
                []
                form.mainWaterSource
                SetMainWaterSource
                Translate.MainWaterSource
            ]

        mainWaterPreparationOptionInput =
            [ viewQuestionLabel language Translate.MainWaterPreparationQuestion
            , viewCheckBoxSelectInput language
                [ Boiled
                , PurificationSolution
                , Filtered
                , Bottled
                , NoWaterPreparationOption
                ]
                []
                form.waterPreparationOption
                SetWaterPreparationOption
                Translate.MainWaterPreparationOption
            ]

        soapInTheHouseUpdateFunc value form_ =
            { form_ | soapInTheHouse = Just value }

        soapInTheHouseInput =
            [ viewQuestionLabel language <| Translate.NutritionHygieneSignQuestion SoapInTheHouse
            , viewBoolInput language
                form.soapInTheHouse
                (SetHygieneBoolInput soapInTheHouseUpdateFunc)
                "soap-in-the-house"
                Nothing
            ]

        washHandsBeforeFeedingUpdateFunc value form_ =
            { form_ | washHandsBeforeFeeding = Just value }

        washHandsBeforeFeedingInput =
            [ viewQuestionLabel language <| Translate.NutritionHygieneSignQuestion WashHandsBeforeFeeding
            , viewBoolInput language
                form.washHandsBeforeFeeding
                (SetHygieneBoolInput washHandsBeforeFeedingUpdateFunc)
                "wash-hands-before-feeding"
                Nothing
            ]

        foodCoveredUpdateFunc value form_ =
            { form_ | foodCovered = Just value }

        foodCoveredInput =
            [ viewQuestionLabel language <| Translate.NutritionHygieneSignQuestion FoodCovered
            , viewBoolInput language
                form.foodCovered
                (SetHygieneBoolInput foodCoveredUpdateFunc)
                "food-covered"
                Nothing
            ]

        content =
            mainWaterSourceInput
                ++ mainWaterPreparationOptionInput
                ++ soapInTheHouseInput
                ++ washHandsBeforeFeedingInput
                ++ foodCoveredInput
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            content
                ++ viewAction language (SaveHygiene assembled.participant.person assembled.measurements.hygiene) disabled
        ]
    ]


viewFoodSecurityContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionFoodSecurityForm -> List (Html Msg)
viewFoodSecurityContent language currentDate assembled db foodSecurityForm =
    let
        form =
            assembled.measurements.foodSecurity
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionFoodSecurityFormWithDefault foodSecurityForm

        totalTasks =
            2

        tasksCompleted =
            taskCompleted form.householdGotFood
                + taskCompleted form.mainIncomeSource

        disabled =
            tasksCompleted /= totalTasks

        mainIncomeSourceInput =
            [ viewQuestionLabel language Translate.MainIncomeSourceQuestion
            , viewCheckBoxSelectInput language
                [ HomeBasedAgriculture
                , CommercialAgriculture
                , PublicEmployee
                , PrivateBusinessEmpployee
                ]
                []
                form.mainIncomeSource
                SetMainIncomeSource
                Translate.MainIncomeSource
            ]

        householdGotFoodUpdateFunc value form_ =
            { form_ | householdGotFood = Just value }

        householdGotFoodInput =
            [ viewQuestionLabel language <| Translate.NutritionFoodSecuritySignQuestion HouseholdGotFood
            , viewBoolInput language
                form.householdGotFood
                (SetFoodSecurityBoolInput householdGotFoodUpdateFunc)
                "household-got-fFood"
                Nothing
            ]

        content =
            householdGotFoodInput
                ++ mainIncomeSourceInput
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            content
                ++ viewAction language (SaveFoodSecurity assembled.participant.person assembled.measurements.foodSecurity) disabled
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
