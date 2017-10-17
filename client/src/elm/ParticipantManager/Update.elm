module ParticipantManager.Update exposing (update, subscriptions)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict exposing (Dict)
import Drupal.Restful exposing (fromEntityId)
import EveryDict
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import HttpBuilder exposing (get, withJsonBody, withQueryParams)
import Pages.Activities.Update
import Pages.Activity.Update
import Pages.Participant.Update
import Pages.Participants.Update
import Participant.Model exposing (Participant(..), ParticipantId(..))
import ParticipantManager.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (User)
import Utils.WebData exposing (sendWithHandler)


update : Date -> BackendUrl -> String -> User -> Language -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update currentDate backendUrl accessToken user language msg model =
    Debug.crash "redo"



{-
   case msg of
       MsgPagesActivities subMsg ->
           let
               ( subModel, subCmd, redirectPage ) =
                   Pages.Activities.Update.update backendUrl accessToken user subMsg (unwrapParticipantsDict model.participants) model.activitiesPage
           in
               ( { model | activitiesPage = subModel }
               , Cmd.map MsgPagesActivities subCmd
               , redirectPage
               )

       MsgPagesActivity subMsg ->
           let
               ( participantUpdated, updatedActivityPage, subCmd, redirectPage ) =
                   Pages.Activity.Update.update backendUrl accessToken user language (unwrapParticipantsDict model.participants) subMsg model.activityPage

               updatedModel =
                   case participantUpdated of
                       Just ( participantId, participant, updatedMeasurements ) ->
                           case Dict.get participantId model.participantPage of
                               Just participantPage ->
                                   let
                                       updatedParticipantPage =
                                           { participantPage | measurements = updatedMeasurements }
                                   in
                                       { model
                                           | activityPage = updatedActivityPage
                                           , participants = Dict.insert participantId (Success participant) model.participants
                                           , participantPage = Dict.insert participantId updatedParticipantPage model.participantPage
                                       }

                               Nothing ->
                                   { model
                                       | activityPage = updatedActivityPage
                                       , participants = Dict.insert participantId (Success participant) model.participants
                                   }

                       Nothing ->
                           { model | activityPage = updatedActivityPage }
           in
               ( updatedModel
               , Cmd.map MsgPagesActivity subCmd
               , redirectPage
               )

       MsgPagesParticipant id subMsg ->
           let
               participantEntry =
                   getParticipant id model
           in
               case participantEntry of
                   Success participant ->
                       let
                           participantModel =
                               Maybe.map identity (Dict.get id model.participantPage)
                                   |> Maybe.withDefault (Pages.Participant.Update.init participant)

                           ( participantUpdated, subModel, subCmd, redirectPage ) =
                               Pages.Participant.Update.update backendUrl accessToken user language ( id, participant ) subMsg participantModel
                       in
                           ( { model
                               | participants = Dict.insert id (Success participantUpdated) model.participants
                               , participantPage = Dict.insert id subModel model.participantPage
                             }
                           , Cmd.map (MsgPagesParticipant id) subCmd
                           , redirectPage
                           )

                   _ ->
                       -- We've received a message for a Participant which we either
                       -- aren't subscribed to, or dont' have initial data for yet.
                       -- This normally wouldn't happen, though we may needd to think
                       -- about synchronization between obtaining our initial data and
                       -- possible "pusher" messages. (Could pusher messages sometimes
                       -- arrive before the initial data, and if so, should we ignore
                       -- them or queue them up? We may need server timestamps on the initial
                       -- data and the pusher messages to know.)
                       ( model, Cmd.none, Nothing )

       MsgPagesParticipants subMsg ->
           let
               ( subModel, subCmd, redirectPage ) =
                   Pages.Participants.Update.update backendUrl accessToken user subMsg (unwrapParticipantsDict model.participants) model.participantsPage
           in
               ( { model | participantsPage = subModel }
               , Cmd.map MsgPagesParticipants subCmd
               , redirectPage
               )
-}


subscriptions : Model -> Page -> Sub Msg
subscriptions model activePage =
    Debug.crash "redo"



{-
   case activePage of
       Participant participantId ->
           case EveryDict.get participantId model.participantPage of
               Just participantPage ->
                   [ Sub.map (MsgPagesParticipant participantId) (Pages.Participant.Update.subscriptions participantPage) ]

               Nothing ->
                   []

       Activity maybeActivityType ->
           case maybeActivityType of
               Just (Child ChildPicture) ->
                   case model.activityPage.selectedParticipant of
                       Just ( participantId, participant ) ->
                           [ Sub.map MsgPagesActivity <| Pages.Activity.Update.subscriptions model.activityPage ( participantId, participant ) ]

                       Nothing ->
                           []

               _ ->
                   []

       _ ->
           []

-}
