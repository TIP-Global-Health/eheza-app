module Pages.AcuteIllnessActivity.View exposing (view)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessActivity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , taskListCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCheckBoxValueInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromPhotoUrl
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        encounter =
            Dict.get id db.acuteIllnessEncounters
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        personId =
            participant
                |> RemoteData.map .person

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        measurements =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked

        personWithMeasurements =
            RemoteData.map (\a b c -> ( a, b, c )) personId
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
    in
    div [ class "page-activity acute-illness" ] <|
        [ viewHeader language id activity
        , viewWebData language (viewContent language currentDate id activity model) identity personWithMeasurements
        ]


viewHeader : Language -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.AcuteIllnessActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Model -> ( PersonId, Person, AcuteIllnessMeasurements ) -> Html Msg
viewContent language currentDate id activity model ( personId, person, measurements ) =
    ((viewPersonDetails language currentDate person |> div [ class "item" ])
        :: viewActivity language currentDate id activity ( personId, measurements ) model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> ( PersonId, AcuteIllnessMeasurements ) -> Model -> List (Html Msg)
viewActivity language currentDate id activity ( personId, measurements ) model =
    case activity of
        AcuteIllnessSymptoms ->
            viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) model.symptomsData

        AcuteIllnessPhysicalExam ->
            [ div [] [ text "AcuteIllnessPhysicalExam here" ] ]

        AcuteIllnessLaboratory ->
            [ div [] [ text "AcuteIllnessLaboratory here" ] ]

        AcuteIllnessExposure ->
            [ div [] [ text "AcuteIllnessExposure here" ] ]


viewAcuteIllnessSymptomsContent : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> SymptomsData -> List (Html Msg)
viewAcuteIllnessSymptomsContent language currentDate id ( personId, measurements ) data =
    let
        activity =
            AcuteIllnessSymptoms

        tasks =
            [ SymptomsGeneral, SymptomsRespiratory, SymptomsGI ]

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        SymptomsGeneral ->
                            ( "symptoms-general", isJust measurements.symptomsGeneral )

                        SymptomsRespiratory ->
                            ( "symptoms-respiratory", isJust measurements.symptomsRespiratory )

                        SymptomsGI ->
                            ( "symptoms-gi", isJust measurements.symptomsGI )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActivePageSymptomsTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.SymptomsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, symptomsTasksCompletedFromTotal measurements data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                SymptomsGeneral ->
                    measurements.symptomsGeneral
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
                        |> viewSymptomsGeneralForm language currentDate measurements

                SymptomsRespiratory ->
                    measurements.symptomsRespiratory
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
                        |> viewSymptomsRespiratoryForm language currentDate measurements

                SymptomsGI ->
                    measurements.symptomsGI
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGIFormWithDefault data.symptomsGIForm
                        |> viewSymptomsGIForm language currentDate measurements

        getNextTask currentTask =
            case currentTask of
                SymptomsGeneral ->
                    [ SymptomsRespiratory, SymptomsGI ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                SymptomsRespiratory ->
                    [ SymptomsGI, SymptomsGeneral ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                SymptomsGI ->
                    [ SymptomsGeneral, SymptomsRespiratory ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                saveMsg =
                    case data.activeTask of
                        SymptomsGeneral ->
                            SaveSymptomsGeneral personId measurements.symptomsGeneral nextTask

                        SymptomsRespiratory ->
                            SaveSymptomsRespiratory personId measurements.symptomsRespiratory nextTask

                        SymptomsGI ->
                            SaveSymptomsGI personId measurements.symptomsGI nextTask
            in
            div [ class "actions symptoms" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveMsg
                    ]
                    [ text <| translate language Translate.Save ]
                ]
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask <|
                tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewSymptomsGeneralForm : Language -> NominalDate -> AcuteIllnessMeasurements -> SymptomsGeneralForm -> Html Msg
viewSymptomsGeneralForm language currentDate measurements form =
    viewCheckBoxValueInput language
        allSymptomsGeneralSigns
        form.signs
        ToggleSymptomsGeneralSign
        SetSymptomsGeneralSignValue
        Translate.SymptomsGeneralSign
        |> List.append
            [ viewQuestionLabel language Translate.PatientGotAnySymptoms
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            ]
        |> div [ class "symptoms-form general" ]


viewSymptomsRespiratoryForm : Language -> NominalDate -> AcuteIllnessMeasurements -> SymptomsRespiratoryForm -> Html Msg
viewSymptomsRespiratoryForm language currentDate measurements form =
    viewCheckBoxValueInput language
        allSymptomsRespiratorySigns
        form.signs
        ToggleSymptomsRespiratorySign
        SetSymptomsRespiratorySignValue
        Translate.SymptomsRespiratorySign
        |> List.append
            [ viewQuestionLabel language Translate.PatientGotAnySymptoms
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            ]
        |> div [ class "symptoms-form respiratory" ]


viewSymptomsGIForm : Language -> NominalDate -> AcuteIllnessMeasurements -> SymptomsGIForm -> Html Msg
viewSymptomsGIForm language currentDate measurements form =
    viewCheckBoxValueInput language
        allSymptomsGISigns
        form.signs
        ToggleSymptomsGISign
        SetSymptomsGISignValue
        Translate.SymptomsGISign
        |> List.append
            [ viewQuestionLabel language Translate.PatientGotAnySymptoms
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            ]
        |> div [ class "symptoms-form gi" ]
