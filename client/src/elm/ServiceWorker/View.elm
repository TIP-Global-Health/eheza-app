module ServiceWorker.View exposing (view, viewIcon)

{-| View functions related to the status of the Service Worker.
-}

import Gizra.Html exposing (emptyNode, showIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..))
import ServiceWorker.Model exposing (Model, Msg(..), NewWorker(..), OutgoingMsg(..))
import Time
import Translate exposing (Language, translate)


view : Time.Posix -> Language -> Model -> Html Msg
view currentTime language model =
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.ServiceWorkerStatus ]
            , span
                [ class "link-back"
                , onClick BackToPinCodePage
                ]
                [ span [ class "icon-back" ] []
                ]
            ]
        , div
            [ class "ui basic segment" ]
            [ viewRegistrationStatus language model
            , viewDeploymentStatus language model
            , showIf model.active <|
                viewUpdateStatus currentTime language model
            ]
        ]


{-| This gives us an icon for our "Version" header on each page.
-}
viewIcon : Model -> Html msg
viewIcon model =
    let
        icon color =
            i
                [ class "download icon"
                , class color
                ]
                []
    in
    case model.newWorker of
        Nothing ->
            emptyNode

        Just Installing ->
            icon "yellow"

        Just Installed ->
            icon "green"

        Just Activating ->
            icon "black"

        Just Activated ->
            icon "black"

        Just Redundant ->
            icon "red"


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
            div []
                [ p [] [ text <| translate language Translate.ServiceWorkerRegErr ]
                , p [] [ text err ]
                ]

        Success _ ->
            p [] [ text <| translate language Translate.ServiceWorkerRegSuccess ]


viewUpdateStatus : Time.Posix -> Language -> Model -> Html Msg
viewUpdateStatus currentTime language model =
    case model.newWorker of
        Nothing ->
            div []
                [ p []
                    [ text <| translate language Translate.ServiceWorkerCurrent ]
                , viewLastChecked language currentTime model.lastUpdateCheck
                , button
                    [ onClick <| SendOutgoingMsg Update
                    , class "ui primary button"
                    ]
                    [ text <| translate language Translate.ServiceWorkerCheckForUpdates ]
                ]

        Just Installing ->
            p [] [ text <| translate language Translate.ServiceWorkerInstalling ]

        Just Installed ->
            if model.active then
                div []
                    [ p []
                        [ text <| translate language Translate.ServiceWorkerInstalled ]
                    , button
                        [ onClick <| SendOutgoingMsg SkipWaiting
                        , class "ui primary button"
                        ]
                        [ text <| translate language Translate.ServiceWorkerSkipWaiting ]
                    ]

            else
                div []
                    [ p [] [ text <| translate language Translate.ServiceWorkerInstalled ]
                    , p [] [ text <| translate language Translate.ServiceWorkerRestarting ]
                    ]

        Just Activating ->
            p [] [ text <| translate language Translate.ServiceWorkerActivating ]

        Just Activated ->
            div []
                [ p [] [ text <| translate language Translate.ServiceWorkerActivated ]
                , p [] [ text <| translate language Translate.ServiceWorkerRestarting ]
                ]

        Just Redundant ->
            div []
                [ p [] [ text <| translate language Translate.ServiceWorkerRedundant ]
                , button
                    [ onClick <| SendOutgoingMsg Update
                    , class "ui primary button"
                    ]
                    [ text <| translate language Translate.ServiceWorkerCheckForUpdates ]
                ]


viewLastChecked : Language -> Time.Posix -> Maybe Time.Posix -> Html msg
viewLastChecked language currentTime checkedTime =
    case checkedTime of
        Just checked ->
            let
                diffInMillis =
                    Time.posixToMillis currentTime - Time.posixToMillis checked

                diffInMinutes =
                    diffInMillis // 60000
            in
            p []
                [ text <| translate language Translate.LastChecked
                , text " "
                , text <| translate language <| Translate.MinutesAgo diffInMinutes
                , text "."
                ]

        Nothing ->
            emptyNode
