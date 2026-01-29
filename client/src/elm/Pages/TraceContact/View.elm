module Pages.TraceContact.View exposing (view)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ContactTraceItem)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (generateFullName)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (SymptomsTask(..))
import Pages.AcuteIllness.Activity.Utils exposing (allSymptomsGISigns, allSymptomsGeneralSigns, allSymptomsRespiratorySigns)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.TraceContact.Model exposing (Model, Msg(..), NoContactReason(..), RecordSymptomsPopupState(..), StepInitiateContactData, StepRecordSymptomsData, SymptomsGIForm, SymptomsGeneralForm, SymptomsRespiratoryForm, TraceContactStep(..))
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomAction
        , viewCustomLabel
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        , viewTasksCount
        )
import RemoteData
import Translate exposing (Language, translate)
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
            Maybe.map (viewPersonDetailsExtended language currentDate)
                tracePerson
                |> Maybe.withDefault (viewContactDetails language currentDate traceContact)

        traceContentStepForm =
            Maybe.map (viewTraceContactStep language currentDate model)
                traceContact
                |> Maybe.withDefault []

        content =
            div [ class "ui unstackable items" ] <|
                div [ class "item" ] personDetails
                    :: traceContentStepForm
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
            ]
        ]


viewContactDetails : Language -> NominalDate -> Maybe ContactTraceItem -> List (Html any)
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


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 140
    , height = 140
    }


viewTraceContactStep : Language -> NominalDate -> Model -> ContactTraceItem -> List (Html Msg)
viewTraceContactStep language currentDate model contact =
    case model.step of
        StepInitiateContact data ->
            viewStepInitiateContact language currentDate contact data

        StepRecordSymptoms data ->
            viewStepRecordSymptoms language currentDate contact data


viewStepInitiateContact : Language -> NominalDate -> ContactTraceItem -> StepInitiateContactData -> List (Html Msg)
viewStepInitiateContact language currentDate contact data =
    let
        instructions =
            p [ class "contact-details" ] <|
                if String.isEmpty contact.phoneNumber then
                    [ text <| translate language Translate.PleaseContact
                    , text " "
                    , span [ class "blue" ] [ text <| generateFullName contact.firstName contact.secondName ]
                    ]

                else
                    [ text <| translate language Translate.PleaseCall
                    , text " "
                    , span [ class "blue" ] [ text <| generateFullName contact.firstName contact.secondName ]
                    , text " "
                    , text <| translate language Translate.At
                    , text " "
                    , span [ class "blue" ] [ text contact.phoneNumber ]
                    ]

        inputs =
            [ viewQuestionLabel language Translate.ContactInitiatedQuestion
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
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            instructions
                :: inputs
                ++ [ viewSaveAction language (SaveStepInitiateContact contact) (tasksCompleted /= totalTasks) ]
        ]
    ]


viewStepRecordSymptoms : Language -> NominalDate -> ContactTraceItem -> StepRecordSymptomsData -> List (Html Msg)
viewStepRecordSymptoms language currentDate contact data =
    let
        tasks =
            [ SymptomsGeneral, SymptomsRespiratory, SymptomsGI ]

        activeTask =
            resolveActiveTask tasks data.activeTask

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

        actions =
            Maybe.map
                (\task ->
                    let
                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

                        saveMsg =
                            case task of
                                SymptomsGeneral ->
                                    SaveSymptomsGeneral contact nextTask

                                SymptomsRespiratory ->
                                    SaveSymptomsRespiratory contact nextTask

                                SymptomsGI ->
                                    SaveSymptomsGI contact nextTask
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
    , viewTasksCount language tasksCompleted totalTasks
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
                , viewCustomAction language (SetActivePage <| UserPage GlobalCaseManagementPage) False Translate.Continue
                ]
        )
        popupState
