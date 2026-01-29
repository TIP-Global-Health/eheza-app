module Pages.HomeVisit.Activity.View exposing (view)

import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Measurement.Model exposing (NutritionCaringForm, NutritionFeedingForm, NutritionFoodSecurityForm, NutritionHygieneForm)
import Measurement.Utils
    exposing
        ( nutritionCaringFormWithDefault
        , nutritionFeedingFormWithDefault
        , nutritionFoodSecurityFormWithDefault
        , nutritionHygieneFormWithDefault
        )
import Measurement.View
    exposing
        ( nutritionCaringInputsAndTasks
        , nutritionFeedingInputsAndTasks
        , nutritionFoodSecurityInputsAndTasks
        , nutritionHygieneInputsAndTasks
        )
import Pages.HomeVisit.Activity.Model exposing (Model, Msg(..))
import Pages.HomeVisit.Encounter.Model exposing (AssembledData)
import Pages.HomeVisit.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( resolveTasksCompletedFromTotal
        , viewPersonDetails
        , viewSaveAction
        , viewTasksCount
        )
import Translate exposing (Language, translate)
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
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| HomeVisitEncounterPage id
            ]
            [ span [ class "icon-back" ] []
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
                |> getMeasurementValueFunc
                |> nutritionFeedingFormWithDefault feedingForm

        ( inputs, tasks ) =
            nutritionFeedingInputsAndTasks language
                currentDate
                assembled.participant.person
                SetFeedingBoolInput
                SetNutritionSupplementType
                SetSachetsPerDay
                db
                form

        ( tasksCompleted, tasksTotal ) =
            resolveTasksCompletedFromTotal tasks

        disabled =
            tasksCompleted /= tasksTotal
    in
    [ viewTasksCount language tasksCompleted tasksTotal
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            inputs
                ++ [ viewSaveAction language
                        (SaveFeeding assembled.participant.person assembled.measurements.feeding)
                        disabled
                   ]
        ]
    ]


viewCaringContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionCaringForm -> List (Html Msg)
viewCaringContent language currentDate assembled db caringForm =
    let
        form =
            assembled.measurements.caring
                |> getMeasurementValueFunc
                |> nutritionCaringFormWithDefault caringForm

        ( inputs, tasks ) =
            nutritionCaringInputsAndTasks language
                currentDate
                SetParentsAliveAndHealthy
                SetNutritionCaringOption
                SetChildClean
                form

        ( tasksCompleted, tasksTotal ) =
            resolveTasksCompletedFromTotal tasks

        disabled =
            tasksCompleted /= tasksTotal
    in
    [ viewTasksCount language tasksCompleted tasksTotal
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            inputs
                ++ [ viewSaveAction language
                        (SaveNutritionCaring assembled.participant.person assembled.measurements.caring)
                        disabled
                   ]
        ]
    ]


viewHygieneContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionHygieneForm -> List (Html Msg)
viewHygieneContent language currentDate assembled db hygieneForm =
    let
        form =
            assembled.measurements.hygiene
                |> getMeasurementValueFunc
                |> nutritionHygieneFormWithDefault hygieneForm

        ( inputs, tasks ) =
            nutritionHygieneInputsAndTasks language
                currentDate
                SetHygieneBoolInput
                SetMainWaterSource
                SetWaterPreparationOption
                form

        ( tasksCompleted, tasksTotal ) =
            resolveTasksCompletedFromTotal tasks

        disabled =
            tasksCompleted /= tasksTotal
    in
    [ viewTasksCount language tasksCompleted tasksTotal
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            inputs
                ++ [ viewSaveAction language
                        (SaveHygiene assembled.participant.person assembled.measurements.hygiene)
                        disabled
                   ]
        ]
    ]


viewFoodSecurityContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NutritionFoodSecurityForm -> List (Html Msg)
viewFoodSecurityContent language currentDate assembled db foodSecurityForm =
    let
        form =
            assembled.measurements.foodSecurity
                |> getMeasurementValueFunc
                |> nutritionFoodSecurityFormWithDefault foodSecurityForm

        ( inputs, tasks ) =
            nutritionFoodSecurityInputsAndTasks language currentDate SetFoodSecurityBoolInput SetMainIncomeSource form

        ( tasksCompleted, tasksTotal ) =
            resolveTasksCompletedFromTotal tasks

        disabled =
            tasksCompleted /= tasksTotal
    in
    [ viewTasksCount language tasksCompleted tasksTotal
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            inputs
                ++ [ viewSaveAction language
                        (SaveFoodSecurity assembled.participant.person assembled.measurements.foodSecurity)
                        disabled
                   ]
        ]
    ]
