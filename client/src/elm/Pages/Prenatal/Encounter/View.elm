module Pages.Prenatal.Encounter.View exposing (generateActivityData, view, viewActionButton, viewMotherAndMeasurements)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (ObstetricHistoryValue)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalActivity.Utils
    exposing
        ( generateHighRiskAlertData
        , generateHighSeverityAlertData
        , getActivityIcon
        )
import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounterType(..)
        , PrenatalProgressReportInitiator(..)
        , RecordPreganancyInitiator(..)
        )
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Utils exposing (activityCompleted, expectActivity, noDangerSigns)
import Pages.Prenatal.Encounter.Model exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.Utils exposing (undeterminedPostpartumDiagnoses)
import Pages.Prenatal.View exposing (customWarningPopup, viewPauseEncounterButton)
import Pages.Utils exposing (viewConfirmationDialog, viewEndEncounterButtonCustomColor, viewPersonDetails, viewReportLink)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Site -> EverySet SiteFeature -> PrenatalEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate site features id isChw db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site features id isChw model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Site -> EverySet SiteFeature -> PrenatalEncounterId -> Bool -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate site features id isChw model assembled =
    let
        header =
            viewHeader language isChw assembled

        content =
            viewContent language currentDate site features isChw assembled model

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewConfirmationDialog language
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
            viewUndeterminedDiagnosesWarningPopup language currentDate site features assembled model
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
            ]
        ]


viewContent : Language -> NominalDate -> Site -> EverySet SiteFeature -> Bool -> AssembledData -> Model -> Html Msg
viewContent language currentDate site features isChw assembled model =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate isChw assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewMainPageContent language currentDate site features assembled model


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


viewUndeterminedDiagnosesWarningPopup : Language -> NominalDate -> Site -> EverySet SiteFeature -> AssembledData -> Model -> Maybe (Html Msg)
viewUndeterminedDiagnosesWarningPopup language currentDate site features assembled model =
    if assembled.encounter.encounterType /= NursePostpartumEncounter || model.undeterminedDiagnosesWarningAcknowledged then
        Nothing

    else
        let
            ( completedActivities, pendingActivities ) =
                getAllActivities features assembled
                    |> List.filter (expectActivity currentDate site features assembled)
                    |> List.partition (activityCompleted currentDate site features assembled)
        in
        if List.length pendingActivities > 0 || List.member NextSteps completedActivities then
            Nothing

        else
            let
                diagnoses =
                    EverySet.toList assembled.encounter.diagnoses

                ( undetermined, _ ) =
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

        alertsDialogSection =
            alertsDialogData
                |> Maybe.map
                    (\( isDialogOpen, setAlertsDialogStateMsg ) ->
                        let
                            firstNurseEncounterMeasurements =
                                getFirstNurseEncounterMeasurements isChw assembled

                            highRiskAlertsData =
                                allHighRiskFactors
                                    |> List.filterMap (generateHighRiskAlertData language firstNurseEncounterMeasurements)

                            highSeverityAlertsData =
                                allHighSeverityAlerts
                                    |> List.filterMap (generateHighSeverityAlertData language currentDate isChw assembled)

                            recurringHighSeverityAlertsData =
                                allRecurringHighSeverityAlerts
                                    |> List.map (generateRecurringHighSeverityAlertData language currentDate isChw assembled)
                                    |> List.filter (List.isEmpty >> not)

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
                    |> List.concatMap viewRecurringHighSeverityAlert

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


viewMainPageContent : Language -> NominalDate -> Site -> EverySet SiteFeature -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate site features assembled model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities features assembled
                |> List.filter (expectActivity currentDate site features assembled)
                |> List.partition (activityCompleted currentDate site features assembled)

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

                navigationPage =
                    case activity of
                        PregnancyOutcome ->
                            PregnancyOutcomePage (InitiatorPostpartumEncounter assembled.id) assembled.encounter.participant

                        _ ->
                            PrenatalActivityPage assembled.id activity
            in
            activityCard language label icon (SetActivePage <| UserPage navigationPage)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        innerContent =
            if model.selectedTab == Reports then
                div [ class "reports-wrapper" ]
                    [ viewReportLink language
                        Translate.ClinicalProgressReport
                        (SetActivePage <|
                            UserPage <|
                                ClinicalProgressReportPage (InitiatorEncounterPage assembled.id) assembled.id
                        )
                    , viewReportLink language
                        Translate.DemographicsReport
                        (SetActivePage <|
                            UserPage <|
                                DemographicsReportPage (InitiatorEncounterPage assembled.id) assembled.participant.person
                        )
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
                    "primary"
                    pendingActivities
                    completedActivities
                    -- When pausing, we close the encounter.
                    -- Entering lab results is available from
                    -- Case management page.
                    CloseEncounter
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
            ( Translate.PrenatalActivityTitle activity, getActivityIcon activity )
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


viewActionButton : Language -> String -> List PrenatalActivity -> List PrenatalActivity -> msg -> (Bool -> msg) -> AssembledData -> Html msg
viewActionButton language buttonColor pendingActivities completedActivities pauseMsg setDialogStateMsg assembled =
    let
        enabled =
            if emergencyReferalRequired assembled then
                List.member NextSteps completedActivities

            else
                case pendingActivities of
                    -- Either all activities are completed
                    [] ->
                        True

                    -- Or only one non mandatory activity remains
                    [ PrenatalPhoto ] ->
                        True

                    _ ->
                        False
    in
    if secondPhaseRequired assembled then
        viewPauseEncounterButton language buttonColor enabled pauseMsg

    else
        viewEndEncounterButtonCustomColor language buttonColor enabled (setDialogStateMsg True)
