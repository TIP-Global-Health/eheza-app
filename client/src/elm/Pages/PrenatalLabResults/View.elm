module Pages.PrenatalLabResults.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
import Backend.PrenatalActivity.Utils exposing (getActivityIcon)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Date exposing (Unit(..))
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import Pages.PrenatalLabResults.Model exposing (..)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id db model assembled =
    div [ class "page-activity prenatal" ] <|
        [ viewHeader language id assembled
        , viewContent language currentDate db model assembled
        ]


viewHeader : Language -> PrenatalEncounterId -> AssembledData -> Html Msg
viewHeader language id assembled =
    let
        ( label, icon ) =
            ( Translate.PrenatalActivitiesTitle Laboratory, getActivityIcon Laboratory )
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language label ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate db model assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled Nothing
            ++ viewLabResults language currentDate assembled model


viewLabResults : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewLabResults language currentDate assembled model =
    -- let
    --     personId =
    --         assembled.participant.person
    --
    --     person =
    --         assembled.person
    --
    --     measurements =
    --         assembled.measurements
    --
    --     tasks =
    --         List.filter (expectLaboratoryTask currentDate assembled) laboratoryTasks
    --
    --     activeTask =
    --         Maybe.Extra.or data.activeTask (List.head tasks)
    --
    --     viewTask task =
    --         let
    --             ( iconClass, isCompleted ) =
    --                 case task of
    --                     TaskHIVTest ->
    --                         ( "laboratory-hiv"
    --                         , isJust measurements.hivTest
    --                         )
    --
    --                     TaskSyphilisTest ->
    --                         ( "laboratory-syphilis"
    --                         , isJust measurements.syphilisTest
    --                         )
    --
    --                     TaskHepatitisBTest ->
    --                         ( "laboratory-hepatitis-b"
    --                         , isJust measurements.hepatitisBTest
    --                         )
    --
    --                     TaskMalariaTest ->
    --                         ( "laboratory-malaria-testing"
    --                         , isJust measurements.malariaTest
    --                         )
    --
    --                     TaskBloodGpRsTest ->
    --                         ( "laboratory-blood-group"
    --                         , isJust measurements.bloodGpRsTest
    --                         )
    --
    --                     TaskUrineDipstickTest ->
    --                         ( "laboratory-urine-dipstick"
    --                         , isJust measurements.urineDipstickTest
    --                         )
    --
    --                     TaskHemoglobinTest ->
    --                         ( "laboratory-hemoglobin"
    --                         , isJust measurements.hemoglobinTest
    --                         )
    --
    --                     TaskRandomBloodSugarTest ->
    --                         ( "laboratory-blood-sugar"
    --                         , isJust measurements.randomBloodSugarTest
    --                         )
    --
    --             isActive =
    --                 activeTask == Just task
    --
    --             attributes =
    --                 classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
    --                     :: (if isActive then
    --                             []
    --
    --                         else
    --                             [ onClick <| SetActiveLaboratoryTask task ]
    --                        )
    --         in
    --         div [ class "column" ]
    --             [ div attributes
    --                 [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
    --                 , text <| translate language (Translate.PrenatalLaboratoryTask task)
    --                 ]
    --             ]
    --
    --     formHtmlAndTasks =
    --         List.map
    --             (\task ->
    --                 ( task
    --                 , case task of
    --                     TaskHIVTest ->
    --                         measurements.hivTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalRDTFormWithDefault data.hivTestForm
    --                             |> viewPrenatalRDTForm language currentDate TaskHIVTest
    --
    --                     TaskSyphilisTest ->
    --                         measurements.syphilisTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalNonRDTFormWithDefault data.syphilisTestForm
    --                             |> viewPrenatalNonRDTForm language currentDate TaskSyphilisTest
    --
    --                     TaskHepatitisBTest ->
    --                         measurements.hepatitisBTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalNonRDTFormWithDefault data.hepatitisBTestForm
    --                             |> viewPrenatalNonRDTForm language currentDate TaskHepatitisBTest
    --
    --                     TaskMalariaTest ->
    --                         measurements.malariaTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalRDTFormWithDefault data.malariaTestForm
    --                             |> viewPrenatalRDTForm language currentDate TaskMalariaTest
    --
    --                     TaskBloodGpRsTest ->
    --                         measurements.bloodGpRsTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalNonRDTFormWithDefault data.bloodGpRsTestForm
    --                             |> viewPrenatalNonRDTForm language currentDate TaskBloodGpRsTest
    --
    --                     TaskUrineDipstickTest ->
    --                         measurements.urineDipstickTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalUrineDipstickFormWithDefault data.urineDipstickTestForm
    --                             |> viewPrenatalUrineDipstickForm language currentDate
    --
    --                     TaskHemoglobinTest ->
    --                         measurements.hemoglobinTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalNonRDTFormWithDefault data.hemoglobinTestForm
    --                             |> viewPrenatalNonRDTForm language currentDate TaskHemoglobinTest
    --
    --                     TaskRandomBloodSugarTest ->
    --                         measurements.randomBloodSugarTest
    --                             |> getMeasurementValueFunc
    --                             |> prenatalNonRDTFormWithDefault data.randomBloodSugarTestForm
    --                             |> viewPrenatalNonRDTForm language currentDate TaskRandomBloodSugarTest
    --                 )
    --             )
    --             tasks
    --             |> Dict.fromList
    --
    --     tasksCompletedFromTotalDict =
    --         Dict.map (\_ ( _, completed, total ) -> ( completed, total ))
    --             formHtmlAndTasks
    --
    --     ( viewForm, tasksCompleted, totalTasks ) =
    --         Maybe.andThen
    --             (\task -> Dict.get task formHtmlAndTasks)
    --             activeTask
    --             |> Maybe.withDefault ( emptyNode, 0, 0 )
    --
    --     nextTask =
    --         List.filter
    --             (\task ->
    --                 (Just task /= activeTask)
    --                     && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
    --             )
    --             tasks
    --             |> List.head
    --
    --     actions =
    --         Maybe.map
    --             (\task ->
    --                 let
    --                     saveMsg =
    --                         case task of
    --                             TaskHIVTest ->
    --                                 SaveHIVTest personId measurements.hivTest nextTask
    --
    --                             TaskSyphilisTest ->
    --                                 SaveSyphilisTest personId measurements.syphilisTest nextTask
    --
    --                             TaskHepatitisBTest ->
    --                                 SaveHepatitisBTest personId measurements.hepatitisBTest nextTask
    --
    --                             TaskMalariaTest ->
    --                                 SaveMalariaTest personId measurements.malariaTest nextTask
    --
    --                             TaskBloodGpRsTest ->
    --                                 SaveBloodGpRsTest personId measurements.bloodGpRsTest nextTask
    --
    --                             TaskUrineDipstickTest ->
    --                                 SaveUrineDipstickTest personId measurements.urineDipstickTest nextTask
    --
    --                             TaskHemoglobinTest ->
    --                                 SaveHemoglobinTest personId measurements.hemoglobinTest nextTask
    --
    --                             TaskRandomBloodSugarTest ->
    --                                 SaveRandomBloodSugarTest personId measurements.randomBloodSugarTest nextTask
    --                 in
    --                 viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
    --             )
    --             activeTask
    --             |> Maybe.withDefault emptyNode
    -- in
    -- [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
    --     [ div [ class "ui five column grid" ] <|
    --         List.map viewTask tasks
    --     ]
    -- , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    -- , div [ class "ui full segment" ]
    --     [ div [ class "full content" ] <|
    --         [ viewForm, actions ]
    --     ]
    -- ]
    [ text "viewLabResults" ]
