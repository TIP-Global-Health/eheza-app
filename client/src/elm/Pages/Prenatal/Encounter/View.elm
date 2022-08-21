module Pages.Prenatal.Encounter.View exposing (generateActivityData, view, viewActionButton, viewMotherAndMeasurements)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (ObstetricHistoryValue, PrenatalMeasurements)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalActivity.Utils
    exposing
        ( generateHighRiskAlertData
        , generateHighSeverityAlertData
        , getActivityIcon
        )
import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounter
        , PrenatalEncounterType(..)
        , PrenatalProgressReportInitiator(..)
        , RecordPreganancyInitiator(..)
        )
import Date exposing (Interval(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Utils exposing (activityCompleted, expectActivity, noDangerSigns)
import Pages.Prenatal.Encounter.Model exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.Utils exposing (undeterminedPostpartumDiagnoses)
import Pages.Prenatal.View exposing (customWarningPopup, viewPauseEncounterButton)
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> Bool -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw model assembled =
    let
        header =
            viewHeader language isChw assembled

        content =
            viewContent language currentDate isChw assembled model

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        CloseEncounter
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-encounter prenatal" ]
        [ header
        , content
        , viewModal <|
            viewChwWarningPopup language assembled model
        , viewModal endEncounterDialog
        , viewModal <|
            viewUndeterminedDiagnosesWarningPopup language currentDate assembled model
        ]


viewHeader : Language -> Bool -> AssembledData -> Html Msg
viewHeader language isChw assembled =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel AntenatalEncounter isChw ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage InitiatorParticipantsPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Bool -> AssembledData -> Model -> Html Msg
viewContent language currentDate isChw assembled model =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate isChw assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewMainPageContent language currentDate assembled model


viewChwWarningPopup : Language -> AssembledData -> Model -> Maybe (Html Msg)
viewChwWarningPopup language assembled model =
    if model.showWarningForChw then
        Just <|
            div [ class "ui tiny active modal" ]
                [ div [ class "content" ]
                    [ span [ class "person-name" ] [ text assembled.person.name ]
                    , text " "
                    , text <| translate language Translate.PatientNotYetSeenAtHCLabel
                    , text "."
                    ]
                , div [ class "actions" ]
                    [ div [ class "two ui buttons" ]
                        [ button
                            [ class "ui primary fluid button"
                            , onClick <| SetChwWarningVisible False
                            ]
                            [ text <| translate language Translate.OK ]
                        ]
                    ]
                ]

    else
        Nothing


viewUndeterminedDiagnosesWarningPopup : Language -> NominalDate -> AssembledData -> Model -> Maybe (Html Msg)
viewUndeterminedDiagnosesWarningPopup language currentDate assembled model =
    if assembled.encounter.encounterType /= NursePostpartumEncounter || model.undeterminedDiagnosesWarningAcknowledged then
        Nothing

    else
        let
            ( completedActivities, pendingActivities ) =
                getAllActivities assembled
                    |> List.filter (expectActivity currentDate assembled)
                    |> List.partition (activityCompleted currentDate assembled)
        in
        if List.length pendingActivities > 0 || List.member NextSteps completedActivities then
            Nothing

        else
            let
                diagnoses =
                    EverySet.toList assembled.encounter.diagnoses

                ( undetermined, determined ) =
                    List.partition (\diagnosis -> List.member diagnosis undeterminedPostpartumDiagnoses) diagnoses
            in
            if List.isEmpty undetermined then
                Nothing

            else
                let
                    undeterminedDiagnoses =
                        List.map (Translate.PrenatalDiagnosisNonUrgentMessage >> translate language) undetermined
                            |> String.join ", "

                    message =
                        div [ class "top-message" ]
                            [ p []
                                [ text <| translate language Translate.UndeterminedDiagnoses
                                , text " - "
                                , text undeterminedDiagnoses
                                ]
                            , p [] [ text <| translate language Translate.FollowPostpartumProtocols ]
                            ]
                in
                Just <| customWarningPopup language ( message, emptyNode, UndeterminedDiagnosesWarningAcknowledged )


viewMotherAndMeasurements : Language -> NominalDate -> Bool -> AssembledData -> Maybe ( Bool, Bool -> msg ) -> List (Html msg)
viewMotherAndMeasurements language currentDate isChw assembled alertsDialogData =
    [ viewMotherDetails language currentDate isChw assembled alertsDialogData
    , viewIndicators language currentDate assembled.globalLmpDate assembled.globalObstetricHistory
    ]


viewMotherDetails : Language -> NominalDate -> Bool -> AssembledData -> Maybe ( Bool, Bool -> msg ) -> Html msg
viewMotherDetails language currentDate isChw assembled alertsDialogData =
    let
        mother =
            assembled.person

        firstEncounterMeasurements =
            getFirstEncounterMeasurements isChw assembled

        highRiskAlertsData =
            allHighRiskFactors
                |> List.filterMap (generateHighRiskAlertData language firstEncounterMeasurements)

        highSeverityAlertsData =
            allHighSeverityAlerts
                |> List.filterMap (generateHighSeverityAlertData language currentDate isChw assembled)

        recurringHighSeverityAlertsData =
            allRecurringHighSeverityAlerts
                |> List.map (generateRecurringHighSeverityAlertData language currentDate isChw assembled)
                |> List.filter (List.isEmpty >> not)

        alertsDialogSection =
            alertsDialogData
                |> Maybe.map
                    (\( isDialogOpen, setAlertsDialogStateMsg ) ->
                        let
                            alertSign =
                                if List.isEmpty highRiskAlertsData && List.isEmpty highSeverityAlertsData && List.isEmpty recurringHighSeverityAlertsData then
                                    emptyNode

                                else
                                    div
                                        [ class "alerts"
                                        , onClick <| setAlertsDialogStateMsg True
                                        ]
                                        [ img [ src "assets/images/exclamation-red.png" ] [] ]

                            dialog =
                                viewModal <|
                                    alertsDialog language
                                        highRiskAlertsData
                                        highSeverityAlertsData
                                        recurringHighSeverityAlertsData
                                        isDialogOpen
                                        setAlertsDialogStateMsg
                        in
                        [ alertSign
                        , dialog
                        ]
                    )
                |> Maybe.withDefault []
    in
    div [ class "item" ] <|
        viewPersonDetails language currentDate mother Nothing
            ++ alertsDialogSection


alertsDialog :
    Language
    -> List String
    -> List ( String, String )
    -> List (List ( String, String, String ))
    -> Bool
    -> (Bool -> msg)
    -> Maybe (Html msg)
alertsDialog language highRiskAlertsData highSeverityAlertsData recurringHighSeverityAlertsData isOpen setAlertsDialogStateMsg =
    if isOpen then
        let
            sectionLabel title =
                div [ class "section-label-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "section-label" ] [ text <| translate language title ++ ":" ]
                    ]

            viewAlertWithValue message value =
                div [ class "alert" ]
                    [ span [ class "alert-text" ] [ text <| "- " ++ message ++ ":" ]
                    , span [ class "alert-value" ] [ text value ]
                    ]

            viewAlertWithValueAndDate ( message, value, date ) =
                div [ class "alert" ]
                    [ span [ class "alert-text" ] [ text <| "- " ++ message ++ ":" ]
                    , span [ class "alert-value" ] [ text value ]
                    , span [ class "alert-date" ] [ text <| "(" ++ date ++ ")" ]
                    ]

            viewAlertWithoutValue message =
                div [ class "alert" ] [ text <| "- " ++ message ]

            viewHighSeverityAlert ( message, value ) =
                if value == "" then
                    viewAlertWithoutValue message

                else
                    viewAlertWithValue message value

            highRiskAlerts =
                highRiskAlertsData
                    |> List.map viewAlertWithoutValue

            highSeverityAlerts =
                highSeverityAlertsData
                    |> List.map viewHighSeverityAlert

            viewRecurringHighSeverityAlert alertsList =
                List.map viewAlertWithValueAndDate alertsList

            recurringHighSeverityAlerts =
                recurringHighSeverityAlertsData
                    |> List.map viewRecurringHighSeverityAlert
                    |> List.concat

            highSeverityAlertsEmpty =
                List.isEmpty highSeverityAlerts && List.isEmpty recurringHighSeverityAlerts
        in
        Just <|
            div [ class "ui active modal alerts-dialog" ]
                [ div [ class "content" ]
                    [ div [ class "high-risk-alerts" ]
                        [ sectionLabel Translate.HighRiskFactors
                        , highRiskAlerts
                            |> div [ class "section-items" ]
                        ]
                        |> showIf (List.isEmpty highRiskAlerts |> not)
                    , div [ class "high-severity-alerts" ]
                        [ sectionLabel Translate.HighSeverityAlerts
                        , highSeverityAlerts
                            ++ recurringHighSeverityAlerts
                            |> div [ class "section-items" ]
                        ]
                        |> showIf (not highSeverityAlertsEmpty)
                    ]
                , div
                    [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| setAlertsDialogStateMsg False
                        ]
                        [ text <| translate language Translate.Close ]
                    ]
                ]

    else
        Nothing


viewIndicators : Language -> NominalDate -> Maybe NominalDate -> Maybe ObstetricHistoryValue -> Html any
viewIndicators language currentDate lmpDate obstetricHistory =
    let
        ( edd, ega ) =
            generateEDDandEGA language currentDate ( "--/--/----", "----" ) lmpDate

        ( gravida, para ) =
            unwrap
                ( "----", "----" )
                (\value ->
                    ( generateGravida value
                    , generatePara value
                    )
                )
                obstetricHistory
    in
    div [ class "item measurements" ]
        [ div [ class "ui edd" ]
            [ div [ class "label" ] [ text <| translate language Translate.Edd ++ ":" ]
            , div [ class "value" ] [ text edd ]
            ]
        , div [ class "ui ega" ]
            [ div [ class "label" ] [ text <| translate language Translate.Ega ++ ":" ]
            , div [ class "value" ] [ text ega ]
            ]
        , div [ class "ui gravida" ]
            [ div [ class "label" ] [ text <| translate language Translate.Gravida ++ ":" ]
            , div [ class "value" ] [ text gravida ]
            ]
        , div [ class "ui para" ]
            [ div [ class "label" ] [ text <| translate language Translate.Para ++ ":" ]
            , div [ class "value" ] [ text para ]
            ]
        ]


viewMainPageContent : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate assembled model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities assembled
                |> List.filter (expectActivity currentDate assembled)
                |> List.partition (activityCompleted currentDate assembled)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.Reports

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
                ]

        viewCard activity =
            let
                ( label, icon ) =
                    generateActivityData activity assembled

                navigationAction =
                    case activity of
                        PregnancyOutcome ->
                            [ onClick <|
                                SetActivePage <|
                                    UserPage <|
                                        PregnancyOutcomePage (InitiatorPostpartumEncounter assembled.id) assembled.encounter.participant
                            ]

                        _ ->
                            [ onClick <|
                                SetActivePage <|
                                    UserPage <|
                                        PrenatalActivityPage assembled.id activity
                            ]
            in
            div [ class "card" ]
                [ div (class "image" :: navigationAction)
                    [ span [ class <| "icon-task icon-task-" ++ icon ] [] ]
                , div [ class "content" ]
                    [ p [] [ text <| String.toUpper <| translate language label ] ]
                ]

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        viewReportLink labelTransId redirectPage =
            div
                [ class "report-wrapper"
                , onClick <| SetActivePage redirectPage
                ]
                [ div [ class "icon-progress-report" ] []
                , div [ class "report-text" ]
                    [ div [ class "report-label" ] [ text <| translate language labelTransId ]
                    , div [ class "report-link" ] [ text <| translate language Translate.View ]
                    ]
                ]

        innerContent =
            if model.selectedTab == Reports then
                div [ class "reports-wrapper" ]
                    [ viewReportLink Translate.ClinicalProgressReport (UserPage <| ClinicalProgressReportPage (InitiatorEncounterPage assembled.id) assembled.id)
                    , viewReportLink Translate.DemographicsReport (UserPage <| DemographicsReportPage (InitiatorEncounterPage assembled.id) assembled.participant.person)
                    ]

            else
                div [ class "full content" ]
                    [ div [ class "wrap-cards" ]
                        [ div [ class "ui four cards" ] <|
                            if List.isEmpty selectedActivities then
                                [ span [] [ text emptySectionMessage ] ]

                            else
                                List.map viewCard selectedActivities
                        ]
                    ]

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewActionButton language
                    pendingActivities
                    completedActivities
                    -- When pausing, we don't close the encounter,
                    -- as it should happend on second phase.
                    (SetActivePage PinCodePage)
                    SetEndEncounterDialogState
                    assembled
                ]
    in
    [ tabs
    , content
    ]


generateActivityData : PrenatalActivity -> AssembledData -> ( TranslationId, String )
generateActivityData activity assembled =
    let
        default =
            ( Translate.PrenatalActivitiesTitle activity, getActivityIcon activity )
    in
    case activity of
        NextSteps ->
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    default

                NursePostpartumEncounter ->
                    default

                ChwPostpartumEncounter ->
                    default

                -- Other types of CHW encounters
                _ ->
                    if noDangerSigns assembled then
                        ( Translate.AppointmentConfirmation, "appointment-confirmation" )

                    else
                        default

        _ ->
            default


viewActionButton : Language -> List PrenatalActivity -> List PrenatalActivity -> msg -> (Bool -> msg) -> AssembledData -> Html msg
viewActionButton language pendingActivities completedActivities pauseMsg setDialogStateMsg assembled =
    let
        enabled =
            if emergencyReferalRequired assembled then
                List.member NextSteps completedActivities

            else
                case pendingActivities of
                    -- Either all activities are completed
                    [] ->
                        True

                    -- Or only one none mandatory activity remains
                    [ PrenatalPhoto ] ->
                        True

                    _ ->
                        False
    in
    if secondPhaseRequired assembled then
        viewPauseEncounterButton language enabled pauseMsg

    else
        viewEndEncounterButton language enabled setDialogStateMsg
