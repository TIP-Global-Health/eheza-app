module Pages.Activities.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..), emptySummaryByActivity)
import Activity.Utils exposing (getActivityIcon, getAllChildActivitiesExcludingNextSteps, getParticipantCountForActivity)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Session.Model exposing (EditableSession)
import EverySet exposing (EverySet)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import LocalData
import Pages.Activities.Model exposing (DialogType(..), Model, Msg(..), Tab(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (viewConfirmationDialog, viewSkipNCDADialog)
import Translate as Trans exposing (Language, translate)
import Utils.Html exposing (tabItem, viewModal)


view : Language -> Bool -> ( SessionId, EditableSession ) -> Model -> Html Msg
view language isChw ( sessionId, session ) model =
    let
        summary =
            session.summaryByActivity
                |> LocalData.withDefault emptySummaryByActivity

        allActivities =
            getAllChildActivitiesExcludingNextSteps session.offlineSession
                |> List.filter (\activity -> not <| EverySet.member activity model.skippedActivities)
                |> List.map (\activity -> ( getParticipantCountForActivity summary activity, activity ))

        ( pendingActivities_, completedActivities_ ) =
            List.partition (\( completedAndPending, _ ) -> completedAndPending.pending > 0) allActivities

        pendingActivities =
            List.map Tuple.second pendingActivities_

        completedActivities =
            List.filterMap
                (\( completedAndPending, activity ) ->
                    if completedAndPending.pending == 0 && completedAndPending.completed == 0 then
                        -- Do not show activities that sum up to 0/0.
                        Nothing

                    else
                        Just activity
                )
                completedActivities_

        pendingTabTitle =
            translate language <| Trans.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Trans.ActivitiesCompleted <| List.length completedActivities

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        viewCard activity =
            let
                activityTitle =
                    if activity == ChildActivity ChildFbf && session.offlineSession.session.clinicType == Achi then
                        Trans.ActivitityTitleAchi

                    else
                        Trans.ActivitiesTitle activity

                action =
                    if activity == ChildActivity NCDA then
                        SetDialogState <| Just DialogSkipNCDA

                    else
                        SetRedirectPage <| UserPage <| SessionPage sessionId <| ActivityPage activity
            in
            div
                [ class "card" ]
                [ div
                    [ class "image"
                    , onClick action
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div
                    [ class "content" ]
                    [ p []
                        [ text <| String.toUpper <| translate language activityTitle ]
                    , div
                        [ class "ui tiny progress" ]
                        [ div
                            [ class "label" ]
                            [ getParticipantCountForActivity summary activity
                                |> Trans.ReportCompleted
                                |> translate language
                                |> text
                            ]
                        ]
                    ]
                ]

        dialog =
            Maybe.map
                (\state ->
                    case state of
                        DialogEndSession ->
                            viewConfirmationDialog language
                                Trans.AreYouSure
                                Trans.OnceYouEndYourGroupEncounter
                                CloseSession
                                (SetDialogState Nothing)

                        DialogSkipNCDA ->
                            viewSkipNCDADialog language
                                (SetRedirectPage <|
                                    UserPage <|
                                        SessionPage sessionId <|
                                            ActivityPage <|
                                                ChildActivity NCDA
                                )
                                (SkipActivity <| ChildActivity NCDA)
                )
                model.dialogState

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Trans.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Trans.NoActivitiesCompleted )

        goBackPage =
            if isChw then
                UserPage GroupEncounterTypesPage

            else
                UserPage ClinicsPage

        endSessionButton =
            div [ class "actions" ]
                [ button
                    [ class "ui fluid button green"
                    , onClick <| SetRedirectPage <| UserPage ClinicalPage
                    ]
                    [ text <| translate language Trans.EndGroupEncounter ]
                ]
    in
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Trans.Activities ]
            , span
                [ class "link-back"
                , onClick <| SetRedirectPage goBackPage
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            , ul [ class "links-head" ]
                [ li
                    [ onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId AttendancePage ]
                    [ a [] [ span [ class "icon-completed" ] [] ] ]
                , li
                    [ onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId ParticipantsPage ]
                    [ a [] [ span [ class "icon-mother" ] [] ] ]
                , li
                    [ class "active" ]
                    [ a [] [ span [ class "icon-measurements" ] [] ] ]
                ]
            ]
        , tabs
        , div
            [ class "ui full segment" ]
            [ div
                [ class "full content" ]
                [ div [ class "wrap-cards" ]
                    [ div [ class "ui four cards" ] <|
                        if List.isEmpty selectedActivities then
                            [ span [] [ text emptySectionMessage ] ]

                        else
                            List.map viewCard selectedActivities
                    ]
                ]
            , endSessionButton
            ]
        , viewModal dialog
        ]
