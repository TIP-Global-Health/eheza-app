module Pages.Activities.View exposing (view)

import Activity.Model exposing (ActivityListItem)
import Activity.Utils exposing (getActivityList)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import List as List
import Patient.Model exposing (PatientsDict)
import User.Model exposing (User)


view : Date -> User -> PatientsDict -> Html a
view currentDate user patients =
    div [] []


viewActivity : ActivityListItem -> Html a
viewActivity report =
    div [ class "ui card activities__item" ]
        [ a [ href "#" ] [ i [ class (report.activity.icon ++ " icon") ] [] ]
        , div [ class "content" ]
            [ a [ class "header activities__item__title" ] [ text report.activity.name ]
            , div [ class "meta" ] [ text <| toString report.remaining ++ " remaining" ]
            ]
        ]
