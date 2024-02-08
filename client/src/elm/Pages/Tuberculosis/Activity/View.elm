module Pages.Tuberculosis.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import Date
import EverySet
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (isJust)
import Measurement.View
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Activity.Utils exposing (..)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
import Pages.Tuberculosis.Encounter.Utils exposing (generateAssembledData)
import Pages.Utils
    exposing
        ( saveButton
        , taskCompleted
        , viewBoolInput
        , viewCustomBoolInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        )
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> TuberculosisEncounterId
    -> TuberculosisActivity
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> TuberculosisEncounterId -> TuberculosisActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity tuberculosis" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate activity db model assembled
        ]


viewHeader : Language -> TuberculosisEncounterId -> TuberculosisActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.TuberculosisActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| TuberculosisEncounterPage id
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> TuberculosisActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled db model
        )


viewActivity : Language -> NominalDate -> TuberculosisActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        Diagnostics ->
            viewDiagnosticsContent language currentDate assembled model.diagnosticsData

        Medication ->
            -- @todo
            []

        SymptomReview ->
            -- @todo
            []

        NextSteps ->
            -- @todo
            []


viewDiagnosticsContent : Language -> NominalDate -> AssembledData -> DiagnosticsData -> List (Html Msg)
viewDiagnosticsContent language currentDate assembled data =
    let
        form =
            assembled.measurements.diagnostics
                |> getMeasurementValueFunc
                |> diagnosticsFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            let
                ( derivedInputs, derivedTasksCompleted, derivedTotalTasks ) =
                    Maybe.map
                        (\diagnosed ->
                            if diagnosed then
                                ( [ viewQuestionLabel language Translate.TuberculosisLocationQuestion
                                  , viewCustomBoolInput language
                                        form.isPulmonary
                                        (SetDiagnosticsBoolInput
                                            (\value form_ ->
                                                { form_
                                                    | isPulmonary = Just value
                                                    , isPulmonaryDirty = True
                                                }
                                            )
                                        )
                                        "is-pulmonary"
                                        ( Translate.TuberculosisSign TuberculosisPulmonary
                                        , Translate.TuberculosisSign TuberculosisExtrapulmonary
                                        )
                                        "sixteen"
                                  ]
                                , taskCompleted form.isPulmonary
                                , 1
                                )

                            else
                                ( [], 0, 0 )
                        )
                        form.diagnosed
                        |> Maybe.withDefault ( [], 0, 0 )
            in
            ( [ viewQuestionLabel language Translate.TuberculosisDiagnosedQuestion
              , viewBoolInput
                    language
                    form.diagnosed
                    (SetDiagnosticsBoolInput
                        (\value form_ ->
                            { form_
                                | diagnosed = Just value
                                , isPulmonary = Nothing
                                , isPulmonaryDirty = True
                            }
                        )
                    )
                    "diagnosed"
                    Nothing
              ]
                ++ derivedInputs
            , taskCompleted form.diagnosed + derivedTasksCompleted
            , 1 + derivedTotalTasks
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveDiagnostics assembled.participant.person assembled.measurements.diagnostics)
            ]
        ]
    ]
