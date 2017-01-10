module Pages.Activities.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import List as List
import Pages.Activities.Model exposing (ActivityReport, Model, activityList)
import Pages.Activities.Utils exposing (isActivityOpen, isActivityCompleted)


view : Html a
view =
    let
        model =
            activityList
    in
        div []
            [ h2 [ class "ui header" ] [ text "Activities to complete" ]
            , div [ class "ui cards activities activities_todo" ]
                (List.filter isActivityOpen model |> List.map viewActivity)
            , h2 [ class "ui header" ] [ text "Activities completed" ]
            , div [ class "ui cards activities activities_complete" ]
                (List.filter isActivityCompleted model |> List.map viewActivity)
            ]


viewActivity : ActivityReport -> Html a
viewActivity report =
    div [ class "ui card activities__item" ]
        [ a [ href "#" ] [ i [ class (report.activity.icon ++ " icon") ] [] ]
        , div [ class "content" ]
            [ a [ class "header activities__item__title" ] [ text report.activity.name ]
            , div [ class "meta" ] [ text ((toString report.remaining) ++ " remaining") ]
            ]
        ]
