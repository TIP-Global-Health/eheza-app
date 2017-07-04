module Pages.Activities.View exposing (view)

import Activity.Model exposing (ActivityListItem)
import Activity.Utils exposing (getActivityList)
import App.PageType exposing (Page(..))
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Pages.Activities.Model exposing (Model, Msg(..))
import Patient.Model exposing (PatientTypeFilter(..), PatientsDict)
import Patient.View exposing (viewPatientTypeFilter)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)


view : Language -> Date -> User -> PatientsDict -> Model -> Html Msg
view language currentDate user patients model =
    let
        allActivityList =
            getActivityList currentDate model.patientTypeFilter patients

        pendingActivities =
            List.filter (\activity -> activity.remaining > 0) allActivityList

        noPendingActivities =
            List.filter (\activity -> activity.remaining == 0) allActivityList

        pendingActivitiesView =
            if List.isEmpty pendingActivities then
                []
            else
                List.map viewActivity pendingActivities

        noPendingActivitiesView =
            if List.isEmpty noPendingActivities then
                div [] []
            else
                div []
                    [ h2 [ class "ui header activities" ] [ text "Activities completed" ]
                    , div [ class "ui cards activities completed" ] (List.map viewActivity noPendingActivities)
                    ]
    in
        div []
            [ viewPatientTypeFilter language SetPatientTypeFilter model.patientTypeFilter
            , h2 [ class "ui header activities" ] [ text "Activities to complete" ]
            , div [ class "ui cards activities pending" ] pendingActivitiesView
            , noPendingActivitiesView
            ]


viewActivity : ActivityListItem -> Html Msg
viewActivity report =
    let
        redirect =
            onClick <| SetRedirectPage <| Dashboard [ report.activity.activityType ]
    in
        div [ class "ui card activities__item" ]
            [ a
                [ href "#"
                , redirect
                ]
                [ i [ class (report.activity.icon ++ " icon") ] [] ]
            , div [ class "content" ]
                [ a [ class "header activities__item__title", redirect ] [ text report.activity.name ]
                , div [ class "meta" ] [ text <| toString report.remaining ++ " remaining" ]
                ]
            ]
