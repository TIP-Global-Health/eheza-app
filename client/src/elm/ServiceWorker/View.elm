module ServiceWorker.View exposing (view)

{-| View functions related to the status of the Service Worker.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import ServiceWorker.Model exposing (..)
import Translate exposing (Language(..), translate)


view : Language -> Model -> Html Msg
view language model =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.ServiceWorkerStatus ]
            ]
        , div
            [ class "ui basic segment" ]
            (viewDeploymentStatus language model)
        ]


viewDeploymentStatus : Language -> Model -> List (Html Msg)
viewDeploymentStatus language model =
    if model.active then
        [ text <| translate language Translate.ServiceWorkerActive ]

    else
        [ text <| translate language Translate.ServiceWorkerInactive ]
