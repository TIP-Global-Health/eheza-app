module Pages.Patient.View
    exposing
        ( viewChild
        , viewMother
        )

import Activity.Model exposing (ActivityListItem)
import Activity.Utils exposing (getActivityList)
import App.PageType
import Child.Model exposing (Child, ChildId)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mother.Model exposing (Mother, MotherId)
import Pages.Patient.Model exposing (Msg(..))
import Patient.Model exposing (Patient, PatientId, PatientTypeFilter(..), PatientsDict)
import RemoteData exposing (RemoteData(..), WebData)
import User.Model exposing (User)


viewChild : Date -> User -> ChildId -> Child -> WebData Mother -> Html Msg
viewChild currentDate currentUser childId child motherWebData =
    let
        motherInfo =
            case child.motherId of
                Nothing ->
                    div [] [ text "Link to mother" ]

                Just motherId ->
                    case motherWebData of
                        Success mother ->
                            div []
                                [ text <| "Mother: "
                                , a
                                    [ href "#"
                                    , onClick <| SetRedirectPage (App.PageType.Patient motherId)
                                    ]
                                    [ text mother.name ]
                                ]

                        Loading ->
                            div []
                                [ text <| "Mother: "
                                , i [ class "icon loading spinner" ] []
                                ]

                        _ ->
                            div [] []

        patients =
            -- @todo: Add mkChild
            Dict.insert childId ({ info = Patient.Model.PatientChild child }) Dict.empty
    in
        div []
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text child.name ]
                ]
            , div [ class "ui card" ]
                [ img [ src child.image ] []
                , div [ class "content" ] [ motherInfo ]
                ]
            , div
                [ class "ui divider" ]
                []
            , div []
                [ viewActivityCards currentDate currentUser patients Children
                ]
            ]


viewMother : Date -> User -> MotherId -> Mother -> List (WebData ( ChildId, Child )) -> Html Msg
viewMother currentDate currentUser motherId mother children =
    let
        childrenInfo =
            (List.map
                (\childWebData ->
                    case childWebData of
                        Success ( childId, child ) ->
                            li []
                                [ a
                                    [ href "#"
                                    , onClick <| SetRedirectPage (App.PageType.Patient childId)
                                    ]
                                    [ text child.name ]
                                ]

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
                div [] [ text "No children registered in the system" ]
            else
                div []
                    [ text <| "Children: "
                    , ul [] childrenInfo
                    ]

        patients =
            -- @todo: Add mkMother
            Dict.insert motherId ({ info = Patient.Model.PatientMother mother }) Dict.empty
    in
        div []
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text mother.name ]
                ]
            , div []
                [ img [ src mother.image ] []
                ]
            , div
                [ class "ui divider" ]
                []
            , childrenList
            , div
                [ class "ui divider" ]
                []
            , div []
                [ viewActivityCards currentDate currentUser patients Mothers
                ]
            ]



-- @todo: Cleanup code duplication


viewActivityCards : Date -> User -> PatientsDict -> PatientTypeFilter -> Html Msg
viewActivityCards currentDate user patients patientTypeFilter =
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
                List.map viewActivityListItem pendingActivities

        noPendingActivitiesView =
            if List.isEmpty noPendingActivities then
                div [] []
            else
                div []
                    [ h2 [ class "ui header" ] [ text "Activities completed" ]
                    , div [ class "ui cards activities activities_complete" ] (List.map viewActivityListItem noPendingActivities)
                    ]
    in
        div []
            [ h2 [ class "ui header" ] [ text "Activities to complete" ]
            , div [ class "ui cards activities activities_todo" ] pendingActivitiesView
            , noPendingActivitiesView
            ]


viewActivityListItem : ActivityListItem -> Html a
viewActivityListItem report =
    div [ class "ui card activities__item" ]
        [ a [ href "#" ] [ i [ class (report.activity.icon ++ " icon") ] [] ]
        , div [ class "content" ]
            [ a [ class "header activities__item__title" ] [ text report.activity.name ]
            ]
        ]
