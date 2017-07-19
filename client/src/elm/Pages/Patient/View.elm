module Pages.Patient.View
    exposing
        ( viewChild
        , viewMother
        )

import Activity.Model exposing (ActivityListItem, ActivityType(..))
import Activity.Utils exposing (getActivityList)
import App.PageType
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Measurement.View
import Mother.Model exposing (Mother, MotherId)
import Pages.Patient.Model exposing (Model, Msg(..), Tab(..))
import Patient.Model exposing (Patient, PatientId, PatientTypeFilter(..), PatientsDict)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (Language, translate)
import User.Model exposing (User)


viewChild : BackendUrl -> String -> User -> Language -> Date -> WebData Mother -> ( ChildId, Child ) -> Model -> Html Msg
viewChild backendUrl accessToken currentUser language currentDate motherWebData ( childId, child ) model =
    let
        motherInfo =
            case child.motherId of
                Nothing ->
                    span [] [ text <| translate language Trans.LinkToMother ]

                Just motherId ->
                    case motherWebData of
                        Success mother ->
                            span []
                                [ text <| translate language Trans.Mother ++ ": ", text mother.name ]

                        Loading ->
                            span []
                                [ text <| translate language Trans.Mother ++ ": "
                                , i [ class "icon loading spinner" ] []
                                ]

                        _ ->
                            span [] []

        patients =
            -- @todo: Add mkChild
            Dict.insert childId ({ info = Patient.Model.PatientChild child }) Dict.empty
    in
        div [] <|
            [ div [ class "ui segment" ]
                [ div [ class "ui items" ]
                    [ div [ class "item" ]
                        [ div [ class "ui image" ]
                            [ img [ src child.image ]
                                []
                            ]
                        , div [ class "middle aligned content" ]
                            [ div []
                                [ h2 [ class "ui disabled header mother" ]
                                    [ motherInfo ]
                                ]
                            , div []
                                [ h2 [ class "ui header child" ]
                                    [ text <| translate language Trans.Baby ++ ": ", text child.name ]
                                ]
                            , div [ class "meta" ]
                                [ p []
                                    [ text <| translate language Trans.PlaceholderTextGroupDate
                                    , br []
                                        []
                                    , text <| translate language Trans.PlaceholderTextJoined
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "ui segment" ]
                [ div []
                    [ viewActivityCards language currentDate currentUser patients Children model.selectedTab
                    ]
                ]
            , Html.map MsgMeasurement <| Measurement.View.viewChild backendUrl accessToken currentUser language ( childId, child ) model.selectedActivity model.measurements
            ]


viewMother : Language -> Date -> User -> MotherId -> Mother -> List (WebData ( ChildId, Child )) -> Model -> Html Msg
viewMother language currentDate currentUser motherId mother children model =
    let
        childrenInfo =
            (List.map
                (\childWebData ->
                    case childWebData of
                        Success ( childId, child ) ->
                            li []
                                [ text child.name ]

                        Loading ->
                            li []
                                [ i [ class "icon loading spinner" ] []
                                ]

                        _ ->
                            div [] []
                )
                children
            )

        childrenList =
            if List.isEmpty mother.children then
                div [] [ text <| translate language Trans.NoChildrenRegisteredInTheSystem ]
            else
                div []
                    [ text <| translate language Trans.Children ++ ": "
                    , ul [] childrenInfo
                    ]

        patients =
            -- @todo: Add mkMother
            Dict.insert motherId ({ info = Patient.Model.PatientMother mother }) Dict.empty
    in
        div [] <|
            [ div [ class "ui segment" ]
                [ div [ class "ui items" ]
                    [ div [ class "item" ]
                        [ div [ class "ui image" ]
                            [ img [ src mother.image ]
                                []
                            ]
                        , div [ class "middle aligned content" ]
                            [ h2 [ class "ui header mother" ]
                                [ text <| translate language Trans.Mother ++ ": ", text mother.name ]
                            , h2 [ class "ui disabled header child" ]
                                [ childrenList ]
                            , div [ class "meta" ]
                                [ p []
                                    [ text <| translate language Trans.PlaceholderTextGroupDate
                                    , br []
                                        []
                                    , text <| translate language Trans.PlaceholderTextJoined
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "ui segment" ]
                [ div []
                    [ viewActivityCards language currentDate currentUser patients Children model.selectedTab
                    ]
                ]
            ]



-- @todo: Cleanup code duplication


viewActivityCards : Language -> Date -> User -> PatientsDict -> PatientTypeFilter -> Tab -> Html Msg
viewActivityCards language currentDate user patients patientTypeFilter selectedTab =
    let
        allActivityList =
            getActivityList currentDate patientTypeFilter patients

        pendingActivities =
            List.filter (\activity -> activity.remaining > 0) allActivityList

        noPendingActivities =
            List.filter (\activity -> activity.remaining == 0) allActivityList

        pendingActivitiesView =
            if List.isEmpty pendingActivities then
                []
            else
                List.map (viewActivityListItem language) pendingActivities

        noPendingActivitiesView =
            if List.isEmpty noPendingActivities then
                []
            else
                List.map (viewActivityListItem language) noPendingActivities

        activeView =
            if selectedTab == Pending then
                pendingActivitiesView
            else
                noPendingActivitiesView

        tabClass tabType =
            [ ( "item", True )
            , ( "active", selectedTab == tabType )
            ]

        tabItem tabType activitiesList =
            let
                tabTitle =
                    case tabType of
                        Pending ->
                            Trans.ActivitiesToComplete

                        Completed ->
                            Trans.ActivitiesCompleted
            in
                a
                    [ classList <| tabClass tabType
                    , onClick <| SetSelectedTab tabType
                    ]
                    [ text <| translate language <| tabTitle <| List.length activitiesList ]

        tabs =
            div [ class "ui text menu" ]
                [ tabItem Pending pendingActivities
                , tabItem Completed noPendingActivities
                ]
    in
        div [ class "ui tasks segment" ]
            [ tabs
            , div [ class "ui five column grid pending" ] activeView
            ]


viewActivityListItem : Language -> ActivityListItem -> Html Msg
viewActivityListItem language report =
    let
        clickHandler =
            onClick <| SetSelectedActivity (Just <| report.activity.activityType)
    in
        div [ class "column" ]
            [ a
                [ clickHandler
                , class "link-section"
                ]
                [ span [ class ("icon-section icon-" ++ report.activity.icon) ] []
                , text report.activity.name
                ]
            ]
