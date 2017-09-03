module Pages.Activities.View exposing (view)

import Activity.Model exposing (ActivityListItem)
import Activity.Utils exposing (getActivityList)
import App.PageType exposing (Page(..))
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Pages.Activities.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (ParticipantTypeFilter(..), ParticipantsDict)
import Participant.View exposing (viewParticipantTypeFilter)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)
import Utils.Html exposing (tabItem)


view : Language -> Date -> User -> ParticipantsDict -> Model -> List (Html Msg)
view language currentDate user participants model =
    let
        allActivityList =
            getActivityList currentDate model.participantTypeFilter participants

        pendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals > 0)) allActivityList

        noPendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals == 0)) allActivityList

        pendingTabTitle =
            translate language <| Trans.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Trans.ActivitiesCompleted <| List.length noPendingActivities

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        viewCard language identity =
            div
                [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetRedirectPage <| Activity <| Just identity.activity.activityType
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ identity.activity.icon ] [] ]
                , div
                    [ class "content" ]
                    [ p [] [ text <| String.toUpper identity.activity.name ]
                    , div
                        [ class "ui tiny progress" ]
                        [ div
                            [ class "label" ]
                            [ text <| translate language <| Trans.ReportCompleted identity.totals ]
                        ]
                    ]
                ]

        selectedActivies =
            case model.selectedTab of
                Pending ->
                    pendingActivities

                Completed ->
                    noPendingActivities
    in
        [ tabs
        , div
            [ class "ui full segment" ]
            [ div [ class "content" ]
                [ div [ class "ui four cards" ] <|
                    List.map (viewCard language) selectedActivies
                ]
            , div [ class "actions" ]
                [ button
                    [ class "ui fluid button" ]
                    [ text <| translate language Trans.EndSession ]
                ]
            ]
        ]
