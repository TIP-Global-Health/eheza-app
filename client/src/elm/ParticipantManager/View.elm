module ParticipantManager.View
    exposing
        ( viewActivities
        , viewPageParticipant
        , viewParticipants
        )

import App.PageType
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Pages.Activities.View
import Pages.Participant.Model
import Pages.Participant.View
import Pages.Participants.View
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import ParticipantManager.Model exposing (..)
import ParticipantManager.Utils exposing (getChildren, getMother, getParticipant, unwrapParticipantsDict)
import RemoteData exposing (RemoteData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (User)
import Utils.WebData exposing (viewError)


{-| Show all Participants page.
-}
viewParticipants : Language -> Date -> User -> Model -> Html Msg
viewParticipants language currentDate user model =
    let
        participants =
            unwrapParticipantsDict model.participants
    in
        div []
            [ Html.map MsgPagesParticipants <| Pages.Participants.View.view language currentDate user participants model.participantsPage
            ]


{-| Show the Participant page.
-}
viewPageParticipant : BackendUrl -> String -> User -> Language -> Date -> ParticipantId -> Model -> Html Msg
viewPageParticipant backendUrl accessToken user language currentDate id model =
    case getParticipant id model of
        NotAsked ->
            -- This shouldn't happen, but if it does, we provide
            -- a button to load the editor
            div
                [ class "ui button"
                , onClick <| Subscribe id
                ]
                [ text <| translate language Trans.ReloadParticipant ]

        Loading ->
            div [] []

        Failure error ->
            div []
                [ viewError language error
                , div
                    [ class "ui button"
                    , onClick <| Subscribe id
                    ]
                    [ text <| translate language Trans.Retry ]
                ]

        Success participant ->
            let
                participantModel =
                    Maybe.map identity (Dict.get id model.participantPage)
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel
            in
                div [ class "wrap" ] <|
                    viewPageParticipantHeader language ( id, participant )
                        :: (case participant.info of
                                ParticipantChild child ->
                                    let
                                        motherWebData =
                                            getMother child.motherId model
                                    in
                                        List.map (Html.map (MsgPagesParticipant id)) <|
                                            Pages.Participant.View.viewChild backendUrl accessToken user language currentDate motherWebData ( id, child ) participantModel

                                ParticipantMother mother ->
                                    let
                                        childrenWebData =
                                            getChildren mother model
                                    in
                                        List.map (Html.map (MsgPagesParticipant id)) <|
                                            Pages.Participant.View.viewMother backendUrl accessToken language currentDate user id mother childrenWebData participantModel
                           )


viewPageParticipantHeader : Language -> ( ParticipantId, Participant ) -> Html Msg
viewPageParticipantHeader language ( participantId, participant ) =
    let
        viewChild id maybeIndex active =
            let
                attributes =
                    if active then
                        [ class "active" ]
                    else
                        [ onClick <|
                            MsgPagesParticipant id <|
                                Pages.Participant.Model.SetRedirectPage (App.PageType.Participant id)
                        ]
            in
                li attributes
                    [ a [] <|
                        span [ class "icon-baby" ] []
                            :: case maybeIndex of
                                Just index ->
                                    [ span [ class "count" ] [ text <| toString (index + 1) ] ]

                                Nothing ->
                                    []
                    ]

        ( motherAttributes, children ) =
            case participant.info of
                ParticipantChild child ->
                    ( case child.motherId of
                        Just motherId ->
                            [ onClick <|
                                MsgPagesParticipant motherId <|
                                    Pages.Participant.Model.SetRedirectPage (App.PageType.Participant motherId)
                            ]

                        Nothing ->
                            []
                    , [ viewChild 0 Nothing True ]
                    )

                ParticipantMother mother ->
                    ( [ class "active" ]
                    , List.indexedMap (\index childId -> viewChild childId (Just index) False) mother.children
                    )
    in
        div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Trans.Assessment ]
            , a
                [ class "link-back"
                , onClick <|
                    MsgPagesParticipant participantId <|
                        Pages.Participant.Model.SetRedirectPage <|
                            App.PageType.Dashboard []
                ]
                [ span [ class "icon-back" ] [] ]
            , ul
                [ class "links-head" ]
              <|
                li
                    motherAttributes
                    [ a []
                        [ span [ class "icon-mother" ] [] ]
                    ]
                    :: children
            ]


viewActivities : Language -> Date -> User -> Model -> Html Msg
viewActivities language currentDate user model =
    let
        participants =
            unwrapParticipantsDict model.participants
    in
        div []
            [ Html.map MsgPagesActivities <| Pages.Activities.View.view language currentDate user participants model.activitiesPage
            ]
