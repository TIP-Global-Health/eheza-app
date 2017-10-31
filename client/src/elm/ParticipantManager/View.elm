module ParticipantManager.View
    exposing
        ( viewActivities
        , viewPageActivity
        , viewPageParticipant
        , viewParticipants
        )

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityIdentity)
import App.PageType
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Pages.Activities.Model
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participant.View
import Pages.Participants.Model
import Pages.Participants.View
import Participant.Model exposing (Participant(..), ParticipantId(..), ParticipantTypeFilter(..), ParticipantsDict)
import ParticipantManager.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Translate as Trans exposing (Language, translate)
import Utils.WebData exposing (viewError)


{-| Show the Participant page.

This one needs the `currentDate` to calculate ages.

-}
viewPageParticipant : Language -> Date -> ParticipantId -> Model -> Html Msg
viewPageParticipant language currentDate id model =
    case Debug.crash "redo" {- getParticipant id model -} of
        NotAsked ->
            -- This shouldn't happen, but if it does, we provide
            -- a button to load the editor
            div [] [ text "NOt found" ]

        Loading ->
            div [] []

        Failure error ->
            div []
                [ viewError language error
                ]

        Success participant ->
            let
                participantModel =
                    Debug.crash "redo"

                {-
                   Maybe.map identity (Dict.get (Debug.crash "id") model.participantPage)
                       |> Maybe.withDefault (Pages.Participant.Update.init participant)
                -}
            in
                div [ class "wrap" ] <|
                    viewPageParticipantHeader language ( id, participant )
                        :: (case participant of
                                ParticipantChild child ->
                                    let
                                        motherWebData =
                                            Debug.crash "redo"

                                        -- getMother child.motherId model
                                    in
                                        List.map (Html.map (MsgPagesParticipant id)) <|
                                            Pages.Participant.View.viewChild language currentDate motherWebData ( Debug.crash "id", child ) participantModel

                                ParticipantMother mother ->
                                    let
                                        childrenWebData =
                                            Debug.crash "redo"

                                        -- getChildren mother model
                                    in
                                        List.map (Html.map (MsgPagesParticipant id)) <|
                                            Pages.Participant.View.viewMother language (Debug.crash "id") mother childrenWebData participantModel
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
                            MsgPagesParticipant (Debug.crash "id") <|
                                Pages.Participant.Model.SetRedirectPage
                                    (App.PageType.Participant (Debug.crash "id"))
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
            case participant of
                ParticipantChild child ->
                    ( case child.motherId of
                        Just motherId ->
                            [ onClick <|
                                MsgPagesParticipant (Debug.crash "motherId") <|
                                    Pages.Participant.Model.SetRedirectPage (App.PageType.Participant (fromEntityId motherId))
                            ]

                        Nothing ->
                            []
                    , Debug.crash "redo"
                      {- List.indexedMap (\index childId -> viewChild childId (Just index) (childId == participantId)) <|
                         List.sort <|
                             participantId
                                 :: case child.siblingId of
                                     Just siblingId ->
                                         [ Debug.crash " siblingId " ]

                                     Nothing ->
                                         []
                      -}
                    )

                ParticipantMother mother ->
                    ( [ class "active" ]
                    , List.indexedMap (\index childId -> viewChild (fromEntityId childId) (Just index) False) mother.children
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
