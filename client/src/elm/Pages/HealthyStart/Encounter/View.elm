module Pages.HealthyStart.Encounter.View exposing (view)

-- import Pages.HealthyStart.Activity.Utils exposing (activityCompleted, expectActivity, noDangerSigns)
-- import Backend.HealthyStartActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.HealthyStartEncounter.Model
    exposing
        ( HealthyStartEncounterType(..)
        , HealthyStartProgressReportInitiator(..)
        , RecordPreganancyInitiator(..)
        )
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (ObstetricHistoryValue)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model exposing (allHighRiskFactors, allHighSeverityAlerts)
import Backend.PrenatalActivity.Utils
    exposing
        ( generateHighRiskAlertData
        , generateHighSeverityAlertData
        , getActivityIcon
        )
import EverySet
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.HealthyStart.Encounter.Model exposing (..)
import Pages.HealthyStart.Encounter.Utils exposing (..)
import Pages.HealthyStart.Model exposing (AssembledData)
import Pages.HealthyStart.View exposing (customWarningPopup, viewPauseEncounterButton)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Encounter.Utils exposing (generateEDDandEGA, generateGravida, generatePara)
import Pages.Prenatal.Utils exposing (undeterminedPostpartumDiagnoses)
import Pages.Utils exposing (viewConfirmationDialog, viewEndEncounterButtonCustomColor, viewPersonDetails, viewReportLink)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Site -> HealthyStartEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate site id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site id model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Site -> HealthyStartEncounterId -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate site id model assembled =
    let
        header =
            viewHeader language assembled

        content =
            viewContent language currentDate site assembled model

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

        -- @todo
        -- , viewModal <|
        --     viewUndeterminedDiagnosesWarningPopup language currentDate site assembled model
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language assembled =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel HealthyStartEncounter False ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| HealthyStartParticipantPage InitiatorParticipantsPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewContent : Language -> NominalDate -> Site -> AssembledData -> Model -> Html Msg
viewContent language currentDate site assembled model =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewMainPageContent language currentDate site assembled model


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



--
-- viewUndeterminedDiagnosesWarningPopup : Language -> NominalDate -> Site -> AssembledData -> Model -> Maybe (Html Msg)
-- viewUndeterminedDiagnosesWarningPopup language currentDate site assembled model =
--     if assembled.encounter.encounterType /= NursePostpartumEncounter || model.undeterminedDiagnosesWarningAcknowledged then
--         Nothing
--
--     else
--         let
--             ( completedActivities, pendingActivities ) =
--                 getAllActivities assembled
--                     |> List.filter (expectActivity currentDate site assembled)
--                     |> List.partition (activityCompleted currentDate site assembled)
--         in
--         if List.length pendingActivities > 0 || List.member NextSteps completedActivities then
--             Nothing
--
--         else
--             let
--                 diagnoses =
--                     EverySet.toList assembled.encounter.diagnoses
--
--                 ( undetermined, _ ) =
--                     List.partition (\diagnosis -> List.member diagnosis undeterminedPostpartumDiagnoses) diagnoses
--             in
--             if List.isEmpty undetermined then
--                 Nothing
--
--             else
--                 let
--                     undeterminedDiagnoses =
--                         List.map (Translate.HealthyStartDiagnosisNonUrgentMessage >> translate language) undetermined
--                             |> String.join ", "
--
--                     message =
--                         div [ class "top-message" ]
--                             [ p []
--                                 [ text <| translate language Translate.UndeterminedDiagnoses
--                                 , text " - "
--                                 , text undeterminedDiagnoses
--                                 ]
--                             , p [] [ text <| translate language Translate.FollowPostpartumProtocols ]
--                             ]
--                 in
--                 Just <| customWarningPopup language ( message, emptyNode, UndeterminedDiagnosesWarningAcknowledged )
--


viewMotherAndMeasurements : Language -> NominalDate -> AssembledData -> Maybe ( Bool, Bool -> msg ) -> List (Html msg)
viewMotherAndMeasurements language currentDate assembled alertsDialogData =
    [ viewMotherDetails language currentDate assembled alertsDialogData
    , viewIndicators language currentDate assembled.globalLmpDate assembled.globalObstetricHistory
    ]


viewMotherDetails : Language -> NominalDate -> AssembledData -> Maybe ( Bool, Bool -> msg ) -> Html msg
viewMotherDetails language currentDate assembled alertsDialogData =
    let
        mother =
            assembled.person

        alertsDialogSection =
            alertsDialogData
                |> Maybe.map
                    (\( isDialogOpen, setAlertsDialogStateMsg ) ->
                        let
                            firstNurseEncounterMeasurements =
                                getFirstNurseEncounterMeasurements assembled

                            highRiskAlertsData =
                                -- @todo
                                -- allHighRiskFactors
                                --     |> List.filterMap (generateHighRiskAlertData language firstNurseEncounterMeasurements)
                                []

                            highSeverityAlertsData =
                                -- @todo
                                --
                                -- allHighSeverityAlerts
                                --     |> List.filterMap (generateHighSeverityAlertData language currentDate False assembled)
                                []

                            recurringHighSeverityAlertsData =
                                -- @todo
                                -- allRecurringHighSeverityAlerts
                                --     |> List.map (generateRecurringHighSeverityAlertData language currentDate isChw assembled)
                                --     |> List.filter (List.isEmpty >> not)
                                []

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


viewMainPageContent : Language -> NominalDate -> Site -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate site assembled model =
    let
        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
                ]

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.Reports

        ( completedActivities, pendingActivities ) =
            -- @todo
            --     getAllActivities assembled
            --         |> List.filter (expectActivity currentDate site assembled)
            --         |> List.partition (activityCompleted currentDate site assembled)
            ( [], [] )

        --
        --
        --
        -- viewCard activity =
        --     let
        --         ( label, icon ) =
        --             generateActivityData activity assembled
        --
        --         navigationPage =
        --             case activity of
        --                 PregnancyOutcome ->
        --                     PregnancyOutcomePage (InitiatorPostpartumEncounter assembled.id) assembled.encounter.participant
        --
        --                 _ ->
        --                     HealthyStartActivityPage assembled.id activity
        --     in
        --     activityCard language label icon (SetActivePage <| UserPage navigationPage)
        --
        -- ( selectedActivities, emptySectionMessage ) =
        --     case model.selectedTab of
        --         Pending ->
        --             ( pendingActivities, translate language Translate.NoActivitiesPending )
        --
        --         Completed ->
        --             ( completedActivities, translate language Translate.NoActivitiesCompleted )
        --
        --         Reports ->
        --             ( [], "" )
        --
        -- innerContent =
        --     if model.selectedTab == Reports then
        --         div [ class "reports-wrapper" ]
        --             [ viewReportLink language
        --                 Translate.ClinicalProgressReport
        --                 (SetActivePage <|
        --                     UserPage <|
        --                         ClinicalProgressReportPage (InitiatorEncounterPage assembled.id) assembled.id
        --                 )
        --             , viewReportLink language
        --                 Translate.DemographicsReport
        --                 (SetActivePage <|
        --                     UserPage <|
        --                         DemographicsReportPage (InitiatorEncounterPage assembled.id) assembled.participant.person
        --                 )
        --             ]
        --
        --     else
        --         div [ class "full content" ]
        --             [ div [ class "wrap-cards" ]
        --                 [ div [ class "ui four cards" ] <|
        --                     if List.isEmpty selectedActivities then
        --                         [ span [] [ text emptySectionMessage ] ]
        --
        --                     else
        --                         List.map viewCard selectedActivities
        --                 ]
        --             ]
        --
        -- content =
        --     div [ class "ui full segment" ]
        --         [ innerContent
        --         , viewActionButton language
        --             "primary"
        --             pendingActivities
        --             completedActivities
        --             -- When pausing, we close the encounter.
        --             -- Entering lab results is available from
        --             -- Case management page.
        --             CloseEncounter
        --             SetEndEncounterDialogState
        --             assembled
        --         ]
    in
    [ tabs

    -- , content
    ]



--
--
-- generateActivityData : HealthyStartActivity -> AssembledData -> ( TranslationId, String )
-- generateActivityData activity assembled =
--     let
--         default =
--             ( Translate.HealthyStartActivityTitle activity, getActivityIcon activity )
--     in
--     case activity of
--         NextSteps ->
--             case assembled.encounter.encounterType of
--                 NurseEncounter ->
--                     default
--
--                 NursePostpartumEncounter ->
--                     default
--
--                 ChwPostpartumEncounter ->
--                     default
--
--                 -- Other types of CHW encounters
--                 _ ->
--                     if noDangerSigns assembled then
--                         ( Translate.AppointmentConfirmation, "appointment-confirmation" )
--
--                     else
--                         default
--
--         _ ->
--             default
--
--
-- viewActionButton : Language -> String -> List HealthyStartActivity -> List HealthyStartActivity -> msg -> (Bool -> msg) -> AssembledData -> Html msg
-- viewActionButton language buttonColor pendingActivities completedActivities pauseMsg setDialogStateMsg assembled =
--     let
--         enabled =
--             if emergencyReferalRequired assembled then
--                 List.member NextSteps completedActivities
--
--             else
--                 case pendingActivities of
--                     -- Either all activities are completed
--                     [] ->
--                         True
--
--                     -- Or only one non mandatory activity remains
--                     [ HealthyStartPhoto ] ->
--                         True
--
--                     _ ->
--                         False
--     in
--     if secondPhaseRequired assembled then
--         viewPauseEncounterButton language buttonColor enabled pauseMsg
--
--     else
--         viewEndEncounterButtonCustomColor language buttonColor enabled (setDialogStateMsg True)
