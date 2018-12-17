module ServiceWorker.View exposing (view)

{-| View functions related to the status of the Service Worker.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..))
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
            , a
                [ class "link-back"
                , onClick BackToLoginPage
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui basic segment" ]
            [ viewDeploymentStatus language model
            , viewRegistrationStatus language model
            ]
        ]


viewDeploymentStatus : Language -> Model -> Html Msg
viewDeploymentStatus language model =
    if model.active then
        p [] [ text <| translate language Translate.ServiceWorkerActive ]

    else
        p [] [ text <| translate language Translate.ServiceWorkerInactive ]


viewRegistrationStatus : Language -> Model -> Html Msg
viewRegistrationStatus language model =
    case model.registration of
        NotAsked ->
            p [] [ text <| translate language Translate.ServiceWorkerRegNotAsked ]

        Loading ->
            p [] [ text <| translate language Translate.ServiceWorkerRegLoading ]

        Failure err ->
            p [] [ text <| translate language Translate.ServiceWorkerRegErr ]

        Success _ ->
            p [] [ text <| translate language Translate.ServiceWorkerRegSuccess ]
