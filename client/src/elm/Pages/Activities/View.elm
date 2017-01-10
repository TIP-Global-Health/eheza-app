module Pages.Activities.View exposing (view)

import Html exposing (..)
import List as List
import Pages.Activities.Model exposing (Model, Activity)
import Pages.Activities.Utils exposing (isActivityOpen, isActivityCompleted)


view : Model -> Html Msg
view model =
    div []
        [ h2 [ class "ui header" ] [ text "Activities to complete" ]
        , div [ class "ui cards activities activities_todo" ]
            (List.filter isActivityOpen model |> List.map viewActivity)
        , h2 [ class "ui header" ] [ text "Activities completed" ]
        , div [ class "ui cards activities activities_complete" ]
            (List.filter isActivityCompleted model |> List.map viewActivity)
        ]


viewActivity : Activity -> Html Msg
viewActivity activity =
    div [ class "ui card activities__item" ]
        [ a [ href "#" ] [ i [ class (activity.icon ++ " icon") ] [] ]
        , div [ class "content" ]
            [ a [ class "header activities__item__title" ] [ text activity.name ]
            , div [ class "meta" ] [ text (toString activity.remaining) ++ " remaining" ]
            ]
        ]
