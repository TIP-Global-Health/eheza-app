module Pages.TraceContact.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ContactTraceEntry)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (generateFullName)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllnessActivity.Types exposing (SymptomsTask(..))
import Pages.AcuteIllnessActivity.Utils exposing (allSymptomsGISigns, allSymptomsGeneralSigns, allSymptomsRespiratorySigns)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.TraceContact.Model exposing (..)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import Pages.WellChildEncounter.View exposing (thumbnailDimensions, viewPersonDetails)
import RemoteData exposing (RemoteData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage, viewModal)


view : Language -> NominalDate -> AcuteIllnessTraceContactId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        traceContact =
            Dict.get id db.traceContacts
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .value

        tracePerson =
            Maybe.andThen
                (\contact ->
                    Dict.get contact.personId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                )
                traceContact

        personDetails =
            Maybe.map (viewPersonDetails language currentDate)
                tracePerson
                |> Maybe.withDefault (viewContactDetails language currentDate traceContact)

        traceContentStepForm =
            Maybe.map (viewTraceContactStep language currentDate model)
                traceContact
                |> Maybe.withDefault []

        content =
            div [ class "ui unstackable items" ] <|
                [ div [ class "item" ] personDetails ]
                    ++ traceContentStepForm
    in
    div [ class "page-activity trace-contact" ]
        [ viewHeader language
        , content
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CovidContactTracing ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContactDetails : Language -> NominalDate -> Maybe ContactTraceEntry -> List (Html any)
viewContactDetails language currentDate traceContact =
    Maybe.map
        (\contact ->
            let
                name =
                    generateFullName contact.firstName contact.secondName

                genderEntry =
                    viewEntry Translate.GenderLabel (translate language <| Translate.Gender contact.gender)

                viewEntry labelTransId content =
                    p []
                        [ span [ class "label" ] [ text <| translate language labelTransId ++ ": " ]
                        , span [] [ text content ]
                        ]
            in
            [ div [ class "ui image" ]
                [ thumbnailImage "mother" Nothing name thumbnailDimensions.height thumbnailDimensions.width ]
            , div [ class "details" ]
                [ h2 [ class "ui header" ]
                    [ text name ]
                , genderEntry
                ]
            ]
        )
        traceContact
        |> Maybe.withDefault []


viewTraceContactStep : Language -> NominalDate -> Model -> ContactTraceEntry -> List (Html Msg)
viewTraceContactStep language currentDate model contact =
    case model.step of
        StepInitiateContact data ->
            viewStepInitiateContact language currentDate contact data

        StepRecordSymptoms data ->
            viewStepRecordSymptoms language currentDate data


viewStepInitiateContact : Language -> NominalDate -> ContactTraceEntry -> StepInitiateContactData -> List (Html Msg)
viewStepInitiateContact language currentDate contact data =
    let
        instructions =
            p [ class "contact-details" ]
                [ text <| translate language Translate.PleaseCall
                , text " "
                , span [ class "blue" ] [ text <| generateFullName contact.firstName contact.secondName ]
                , text " "
                , text <| translate language Translate.At
                , text " "
                , span [ class "blue" ] [ text contact.phoneNumber ]
                ]

        inputs =
            [ viewQuestionLabel language Translate.ContactInitiatedQuesiton
            , viewBoolInput
                language
                data.contactInitiated
                SetContactInitiated
                ""
                Nothing
            ]
                ++ derivedInput

        derivedInput =
            if data.contactInitiated == Just False then
                [ div [ class "why-not" ]
                    [ viewQuestionLabel language Translate.WhyNot
                    , viewCheckBoxSelectInput language
                        [ ReasonNoAnswer, ReasonWrongContactInfo, ReasonDeclinedFollowUp ]
                        []
                        data.noContactReason
                        SetNoContactReason
                        Translate.NoContactReason
                    ]
                ]

            else
                []

        ( totalTasks, tasksCompleted ) =
            ( 1 + derivedTaskActive
            , taskCompleted data.contactInitiated + derivedTaskCompleted
            )

        ( derivedTaskActive, derivedTaskCompleted ) =
            if data.contactInitiated == Just False then
                ( 1, taskCompleted data.noContactReason )

            else
                ( 0, 0 )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            instructions
                :: inputs
                ++ [ viewSaveAction language (SaveStepInitiateContact contact) (tasksCompleted /= totalTasks) ]
        ]
    ]


viewStepRecordSymptoms : Language -> NominalDate -> StepRecordSymptomsData -> List (Html Msg)
viewStepRecordSymptoms language currentDate data =
    let
        tasks =
            [ SymptomsGeneral, SymptomsRespiratory, SymptomsGI ]

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        SymptomsGeneral ->
                            ( "symptoms-general", data.symptomsGeneralForm.completed )

                        SymptomsRespiratory ->
                            ( "symptoms-respiratory", data.symptomsRespiratoryForm.completed )

                        SymptomsGI ->
                            ( "symptoms-gi", data.symptomsGIForm.completed )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveSymptomsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.SymptomsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    ( task, symptomsTasksCompletedFromTotal data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just SymptomsGeneral ->
                    viewSymptomsGeneralForm language currentDate data.symptomsGeneralForm

                Just SymptomsRespiratory ->
                    viewSymptomsRespiratoryForm language currentDate data.symptomsRespiratoryForm

                Just SymptomsGI ->
                    viewSymptomsGIForm language currentDate data.symptomsGIForm

                Nothing ->
                    emptyNode

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
                                SymptomsGeneral ->
                                    SaveSymptomsGeneral nextTask

                                SymptomsRespiratory ->
                                    SaveSymptomsRespiratory nextTask

                                SymptomsGI ->
                                    SaveSymptomsGI nextTask
                    in
                    viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui three column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    , viewModal <|
        viewWarningPopup language data.popupState
    ]


symptomsTasksCompletedFromTotal : StepRecordSymptomsData -> SymptomsTask -> ( Int, Int )
symptomsTasksCompletedFromTotal data task =
    let
        everySetGotValue set =
            if EverySet.isEmpty set then
                0

            else
                1
    in
    case task of
        SymptomsGeneral ->
            ( everySetGotValue data.symptomsGeneralForm.signs
            , 1
            )

        SymptomsRespiratory ->
            ( everySetGotValue data.symptomsRespiratoryForm.signs
            , 1
            )

        SymptomsGI ->
            ( everySetGotValue data.symptomsGIForm.signs
            , 1
            )


viewSymptomsGeneralForm : Language -> NominalDate -> SymptomsGeneralForm -> Html Msg
viewSymptomsGeneralForm language currentDate form =
    let
        signs =
            Tuple.first allSymptomsGeneralSigns ++ [ Tuple.second allSymptomsGeneralSigns ]
    in
    div [ class "symptoms-form general" ]
        [ viewQuestionLabel language Translate.PatientGotAnySymptoms
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            signs
            []
            (EverySet.toList form.signs)
            Nothing
            ToggleSymptomsGeneralSign
            Translate.SymptomsGeneralSign
        ]


viewSymptomsRespiratoryForm : Language -> NominalDate -> SymptomsRespiratoryForm -> Html Msg
viewSymptomsRespiratoryForm language currentDate form =
    let
        signs =
            Tuple.first allSymptomsRespiratorySigns ++ [ Tuple.second allSymptomsRespiratorySigns ]
    in
    div [ class "symptoms-form respiratory" ]
        [ viewQuestionLabel language Translate.PatientGotAnySymptoms
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            signs
            []
            (EverySet.toList form.signs)
            Nothing
            ToggleSymptomsRespiratorySign
            Translate.SymptomsRespiratorySign
        ]


viewSymptomsGIForm : Language -> NominalDate -> SymptomsGIForm -> Html Msg
viewSymptomsGIForm language currentDate form =
    let
        signs =
            Tuple.first allSymptomsGISigns ++ [ Tuple.second allSymptomsGISigns ]
    in
    div [ class "symptoms-form gi" ]
        [ viewQuestionLabel language Translate.PatientGotAnySymptoms
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
        , viewCheckBoxMultipleSelectInput language
            signs
            []
            (EverySet.toList form.signs)
            Nothing
            ToggleSymptomsGISign
            Translate.SymptomsGISign
        ]


viewWarningPopup : Language -> Maybe RecordSymptomsPopupState -> Maybe (Html Msg)
viewWarningPopup language popupState =
    Maybe.map
        (\state ->
            let
                content =
                    case state of
                        StateSymptomsFound ->
                            [ div [ class "popup-heading-wrapper" ]
                                [ img [ src "assets/images/exclamation-red.png" ] []
                                , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                                ]
                            , div [ class "popup-title" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning DiagnosisCovid19Suspect ]
                            , div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseReferToHCForTesting ]
                            ]

                        StateSymptomsNotFound ->
                            [ div [ class "popup-title" ] [ text <| translate language Translate.PatientShowsNoSignsOfCovid ]
                            , div [ class "popup-action" ] [ text <| translate language Translate.ProvideHealthEducationAndInstructToIsolate ]
                            ]
            in
            div
                [ classList
                    [ ( "ui active modal trace-contact-popup", True )
                    , ( "cyan", state == StateSymptomsNotFound )
                    ]
                ]
                [ div [ class "content" ] content
                , div [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| SetRecordSymptomsPopupState Nothing
                        ]
                        [ text <| translate language Translate.Continue ]
                    ]
                ]
        )
        popupState
