module Pages.Patient.View
    exposing
        ( viewChild
        , viewMother
        , viewSelectedActivity
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
import Measurement.Model
import Measurement.View
import Mother.Model exposing (Mother, MotherId)
import Pages.Patient.Model exposing (ActivityOptions, Model, Msg(..))
import Patient.Model exposing (Patient, PatientId, PatientTypeFilter(..), PatientsDict)
import RemoteData exposing (RemoteData(..), WebData)
import Translate as Trans exposing (Language, translate)
import User.Model exposing (User)


type alias Measurements =
    Measurement.Model.Model


viewChild : BackendUrl -> String -> User -> Language -> Date -> WebData Mother -> ( ChildId, Child ) -> Model -> Html Msg
viewChild backendUrl accessToken currentUser language currentDate motherWebData ( childId, child ) model =
    let
        motherInfo =
            case child.motherId of
                Nothing ->
                    div [] [ text <| translate language Trans.LinkToMother ]

                Just motherId ->
                    case motherWebData of
                        Success mother ->
                            div []
                                [ text <| translate language Trans.Mother ++ ": "
                                , a
                                    [ href "#"
                                    , onClick <| SetRedirectPage (App.PageType.Patient motherId)
                                    ]
                                    [ img [ src mother.image, class "ui avatar image" ] []
                                    , text mother.name
                                    ]
                                ]

                        Loading ->
                            div []
                                [ text <| translate language Trans.Mother ++ ": "
                                , i [ class "icon loading spinner" ] []
                                ]

                        _ ->
                            div [] []

        patients =
            -- @todo: Add mkChild
            Dict.insert childId ({ info = Patient.Model.PatientChild child }) Dict.empty
    in
        div [] <|
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text child.name ]
                ]
            , div [ class "ui grid" ]
                [ div [ class "six wide column" ]
                    [ img [ class "ui medium rounded image", src child.image ] []
                    ]
                , div [ class "content six wide column" ] [ motherInfo ]
                ]
            , div
                [ class "ui divider" ]
                []
            , div []
                [ viewActivityCards language currentDate currentUser patients Children
                ]
            , Html.map MsgMeasurement <| Measurement.View.viewChild backendUrl accessToken currentUser ( childId, child ) model.selectedActivity model.measurements
            ]


viewMother : Language -> Date -> User -> MotherId -> Mother -> List (WebData ( ChildId, Child )) -> Html Msg
viewMother language currentDate currentUser motherId mother children =
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
                                    [ img [ src child.image, class "ui avatar image" ] []
                                    , text child.name
                                    ]
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
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text mother.name ]
                ]
            , div [ class "ui grid" ]
                [ div [ class "six wide column" ]
                    [ img [ class "ui medium rounded image", src mother.image ] []
                    ]
                , div [ class "six wide column" ] [ childrenList ]
                ]
            , div
                [ class "ui divider" ]
                []
            , div []
                [ viewActivityCards language currentDate currentUser patients Mothers
                ]
            , viewSelectedActivity language (Just Pages.Patient.Model.Weight)
            ]



-- @todo: Cleanup code duplication


viewActivityCards : Language -> Date -> User -> PatientsDict -> PatientTypeFilter -> Html Msg
viewActivityCards language currentDate user patients patientTypeFilter =
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
                div [] []
            else
                div []
                    [ h2 [ class "ui header activities" ] [ text <| translate language Trans.ActivitiesCompleted ]
                    , div [ class "ui cards activities completed" ] (List.map (viewActivityListItem language) noPendingActivities)
                    ]
    in
        div []
            [ h2 [ class "ui header activities" ] [ text <| translate language Trans.ActivitiesToComplete ]
            , div [ class "ui cards activities pending" ] pendingActivitiesView
            , noPendingActivitiesView
            ]


viewActivityListItem : Language -> ActivityListItem -> Html Msg
viewActivityListItem language report =
    div [ class "ui card activities__item" ]
        [ a
            [ href "#"

            -- @todo: Need to pass the ActivityType to this function.
            , onClick <| SetSelectedActivity (Just <| Activity.Model.Child Activity.Model.Weight)
            ]
            [ i [ class (report.activity.icon ++ " icon") ] [] ]
        , div [ class "content" ]
            [ a [ class "header activities__item__title" ] [ text report.activity.name ]
            ]
        ]


viewSelectedActivity : Language -> Maybe ActivityOptions -> Html Msg
viewSelectedActivity language activity =
    case activity of
        Just Pages.Patient.Model.Weight ->
            viewWeightEntry language

        Nothing ->
            div [] []



-- @todo: Remove


viewWeightEntry : Language -> Html Msg
viewWeightEntry language =
    div []
        [ div
            [ class "ui divider" ]
            []
        , div
            [ class "ui card"
            , id "weightEntryForm"
            ]
            [ h1
                []
                [ text <| translate language Trans.ActivitiesWeightTitle
                ]
            , span
                []
                [ text <| translate language Trans.ActivitiesWeightHelp ]
            , div
                []
                [ span [] [ text <| translate language Trans.ActivitiesWeightLabel ]
                , input
                    [ type_ "number"
                    , name "weight"
                    , Attr.min "1"
                    , Attr.max "200"
                    ]
                    []
                , span [] [ text <| translate language Trans.KilogramShorthand ]
                ]
            ]
        ]
