module Pages.Participant.View
    exposing
        ( viewChild
        , viewMother
        )

import Activity.Model exposing (ActivityListItem, ActivityType(..))
import Activity.Utils exposing (getActivityList)
import Child.Model exposing (Child, ChildId, Gender(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import Examination.Model exposing (ExaminationChild)
import Examination.Utils exposing (getLastExaminationFromChild)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Measurement.View
import Mother.Model exposing (Mother, MotherId)
import Pages.Participant.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant, ParticipantId, ParticipantTypeFilter(..), ParticipantsDict)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (Language, translate)
import User.Model exposing (User)


viewChild : BackendUrl -> String -> User -> Language -> Date -> WebData Mother -> ( ChildId, Child ) -> Model -> List (Html Msg)
viewChild backendUrl accessToken currentUser language currentDate motherWebData ( childId, child ) model =
    let
        participants =
            -- @todo: Add mkChild
            Dict.insert childId ({ info = Participant.Model.ParticipantChild child }) Dict.empty

        childName =
            translate language <| Trans.BabyName child.name

        motherInfo =
            case child.motherId of
                Nothing ->
                    []

                Just motherId ->
                    case motherWebData of
                        Success mother ->
                            [ text <| translate language <| Trans.MotherName mother.name ]

                        _ ->
                            []

        dateOfBirth =
            text <| translate language <| Trans.ReportDOB "To be implemented"

        age =
            text <| translate language <| Trans.ReportAge "To be implemented"

        gender =
            case child.gender of
                Male ->
                    text <| translate language <| Trans.Male

                Female ->
                    text <| translate language <| Trans.Female

        break =
            br [] []
    in
        div [ class "ui unstackable items" ]
            [ div [ class "item" ]
                [ div [ class "ui image" ]
                    [ img [ src child.image, attribute "alt" childName, width 222, height 222 ] [] ]
                , div [ class "content" ]
                    [ h2 [ class "ui header" ]
                        [ text childName ]
                    , p [] <|
                        motherInfo
                            ++ [ break, dateOfBirth, break, age, break, gender ]
                    ]
                ]
            ]
            :: ((viewActivityCards language currentDate currentUser participants Children model.selectedTab model.selectedActivity)
                    ++ [ Html.map MsgMeasurement <|
                            Measurement.View.viewChild backendUrl accessToken currentUser language ( childId, child ) (getLastExaminationFromChild child) model.selectedActivity model.measurements
                       ]
               )


viewMother : BackendUrl -> String -> Language -> Date -> User -> MotherId -> Mother -> List (WebData ( ChildId, Child )) -> Model -> List (Html Msg)
viewMother backendUrl accessToken language currentDate currentUser motherId mother children model =
    let
        break =
            br [] []

        childrenList =
            List.intersperse break <|
                List.indexedMap
                    (\index childWebData ->
                        case childWebData of
                            Success ( childId, child ) ->
                                text <| (translate language Trans.Baby) ++ " " ++ toString (index + 1) ++ ": " ++ child.name

                            _ ->
                                text ""
                    )
                    children

        participants =
            -- @todo: Add mkMother
            Dict.insert motherId ({ info = Participant.Model.ParticipantMother mother }) Dict.empty
    in
        div [ class "ui unstackable items" ]
            [ div [ class "item" ]
                [ div [ class "ui image" ]
                    [ img [ src mother.image, attribute "alt" mother.name, width 222, height 222 ] [] ]
                , div [ class "content" ]
                    [ h2
                        [ class "ui header" ]
                        [ text mother.name ]
                    , p [] childrenList
                    ]
                ]
            ]
            :: ((viewActivityCards language currentDate currentUser participants Mothers model.selectedTab model.selectedActivity)
                    ++ [ Html.map MsgMeasurement <|
                            Measurement.View.viewMother backendUrl accessToken currentUser language model.selectedActivity model.measurements
                       ]
               )


viewActivityCards : Language -> Date -> User -> ParticipantsDict -> ParticipantTypeFilter -> Tab -> Maybe ActivityType -> List (Html Msg)
viewActivityCards language currentDate user participants participantTypeFilter selectedTab selectedActivity =
    let
        allActivityList =
            getActivityList currentDate participantTypeFilter participants

        pendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals) > 0) allActivityList

        noPendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals) == 0) allActivityList

        pendingActivitiesView =
            if List.isEmpty pendingActivities then
                []
            else
                List.map (viewActivityListItem language selectedActivity) pendingActivities

        noPendingActivitiesView =
            if List.isEmpty noPendingActivities then
                []
            else
                List.map (viewActivityListItem language selectedActivity) noPendingActivities

        activeView =
            div [ class "ui task segment" ]
                [ div [ class "ui five column grid" ] <|
                    if selectedTab == Pending then
                        pendingActivitiesView
                    else
                        noPendingActivitiesView
                ]

        tabClass tabType =
            [ ( "item", True )
            , ( "active", selectedTab == tabType )
            ]

        tabItem tabType activitiesList =
            let
                tabId =
                    (String.toLower <| (toString tabType)) ++ "-tab"

                tabTitle =
                    case tabType of
                        Pending ->
                            Trans.ActivitiesToComplete

                        Completed ->
                            Trans.ActivitiesCompleted
            in
                a
                    [ classList <| tabClass tabType
                    , id tabId
                    , onClick <| SetSelectedTab tabType
                    ]
                    [ text <| translate language <| tabTitle <| List.length activitiesList ]

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem Pending pendingActivities
                , tabItem Completed noPendingActivities
                ]
    in
        [ tabs, activeView ]


viewActivityListItem : Language -> Maybe ActivityType -> ActivityListItem -> Html Msg
viewActivityListItem language selectedActivity report =
    let
        clickHandler =
            onClick <| SetSelectedActivity (Just <| report.activity.activityType)
    in
        div [ class "column" ]
            [ a
                [ clickHandler
                , classList [ ( "link-section", True ), ( "active", selectedActivity == (Just <| report.activity.activityType) ) ]
                ]
                [ span [ class ("icon-section icon-" ++ report.activity.icon) ] []
                , text report.activity.name
                ]
            ]
