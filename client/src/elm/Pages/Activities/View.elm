module Pages.Activities.View exposing (view)

import Activity.Model exposing (ActivityListItem)
import Activity.Utils exposing (getActivityList)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import List as List
import Pages.Activities.Model exposing (Model, Msg(..))
import Patient.Model exposing (PatientTypeFilter(..), PatientsDict)
import Patient.View exposing (viewPatientTypeFilter)
import User.Model exposing (User)


view : Date -> User -> PatientsDict -> Model -> Html Msg
view currentDate user patients model =
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
                    [ h2 [ class "ui header" ] [ text "Activities completed" ]
                    , div [ class "ui cards activities activities_complete" ] (List.map viewActivity noPendingActivities)
                    ]
    in
        div []
            [ viewPatientTypeFilter SetPatientTypeFilter model.patientTypeFilter
            , h2 [ class "ui header" ] [ text "Activities to complete" ]
            , div [ class "ui cards activities activities_todo" ] pendingActivitiesView
            , noPendingActivitiesView
            ]


viewActivity : ActivityListItem -> Html a
viewActivity report =
    div [ class "ui card activities__item" ]
        [ a [ href "#" ] [ i [ class (report.activity.icon ++ " icon") ] [] ]
        , div [ class "content" ]
            [ a [ class "header activities__item__title" ] [ text report.activity.name ]
            , div [ class "meta" ] [ text <| toString report.remaining ++ " remaining" ]
            ]
        ]
