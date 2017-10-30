module Pages.Activities.View exposing (view)

import Activity.Utils exposing (getActivityList, getActivityIcon)
import Backend.Session.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Pages.Activities.Model exposing (Model, Msg(..), Tab(..))
import Pages.Page exposing (Page(..))
import Pages.Utils exposing (DashboardPage(..), viewDashboardPageHeader)
import Participant.Model exposing (ParticipantTypeFilter(..))
import Translate as Trans exposing (translate, Language)
import Utils.Html exposing (tabItem)


view : Language -> EditableSession -> Model -> Html Msg
view language session model =
    let
        allActivityList =
            getActivityList model.participantTypeFilter session

        pendingActivities =
            List.filter (\activity -> (activity.totals.pending > 0)) allActivityList

        noPendingActivities =
            List.filter (\activity -> (activity.totals.pending == 0)) allActivityList

        pendingTabTitle =
            translate language <| Trans.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Trans.ActivitiesCompleted <| List.length noPendingActivities

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        viewCard language item =
            div
                [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetRedirectPage <| ActivityPage item.activityType
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon item.activityType ] [] ]
                , div
                    [ class "content" ]
                    [ p [] [ text <| String.toUpper <| translate language (Trans.ActivitiesTitle item.activityType) ]
                    , div
                        [ class "ui tiny progress" ]
                        [ div
                            [ class "label" ]
                            [ text <| translate language <| Trans.ReportCompleted item.totals ]
                        ]
                    ]
                ]

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Trans.PendingSectionEmpty )

                Completed ->
                    ( noPendingActivities, translate language Trans.CompletedSectionEmpty )
    in
        div
            [ class "wrap wrap-alt" ]
            [ viewDashboardPageHeader SetRedirectPage language ActivitiesDashboard
            , tabs
            , div
                [ class "ui full segment" ]
                [ div [ class "content" ]
                    [ div [ class "ui four cards" ] <|
                        if List.isEmpty selectedActivities then
                            [ span [] [ text emptySectionMessage ] ]
                        else
                            List.map (viewCard language) selectedActivities
                    ]
                , div [ class "actions" ]
                    [ button
                        [ class "ui fluid button" ]
                        [ text <| translate language Trans.EndSession ]
                    ]
                ]
            ]
