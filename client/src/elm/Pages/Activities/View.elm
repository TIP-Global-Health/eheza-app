module Pages.Activities.View exposing (view)

import Activity.Utils exposing (getActivityList)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Pages.Activities.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (ParticipantTypeFilter(..), ParticipantsDict)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)


view : Language -> Date -> User -> ParticipantsDict -> Model -> List (Html Msg)
view language currentDate user participants model =
    let
        allActivityList =
            getActivityList model.participantTypeFilter participants

        pendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals > 0)) allActivityList

        noPendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals == 0)) allActivityList

        tabItem tab activitiesList =
            let
                tabTitle =
                    case tab of
                        Pending ->
                            Trans.ActivitiesToComplete

                        Completed ->
                            Trans.ActivitiesCompleted

                tabClass tab =
                    [ ( "item", True )
                    , ( "active", model.activeTab == tab )
                    ]
            in
                a
                    [ classList <| tabClass tab
                    , onClick <| SetActiveTab tab
                    ]
                    [ text <| translate language <| tabTitle <| List.length activitiesList ]

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem Pending pendingActivities
                , tabItem Completed noPendingActivities
                ]

        viewCard language identity =
            div
                [ class "card" ]
                [ div
                    [ class "image" ]
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
            case model.activeTab of
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
