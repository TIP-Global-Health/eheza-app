port module ParticipantManager.Update exposing (update, subscriptions, dashboardUrlFragment)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import HttpBuilder exposing (get, withJsonBody, withQueryParams)
import Pages.Activities.Update
import Pages.Activity.Update
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participants.Model
import Pages.Participants.Update
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..))
import ParticipantManager.Decoder
    exposing
        ( decodeParticipantFromResponse
        , decodeParticipantsFromResponse
        )
import ParticipantManager.Model exposing (..)
import ParticipantManager.Utils exposing (..)
import Pusher.Decoder exposing (decodePusherEvent)
import Pusher.Model exposing (PusherEventData(..))
import RemoteData exposing (RemoteData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (User)
import Utils.WebData exposing (sendWithHandler)


{-| If we're on the `Dashboard` page, what should we show in the part of the URL
that we'll be asked to decode?
-}
dashboardUrlFragment : Model -> String
dashboardUrlFragment model =
    Pages.Participants.Update.urlFragment model.participantsPage


update : Date -> BackendUrl -> String -> User -> Language -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update currentDate backendUrl accessToken user language msg model =
    case msg of
        Subscribe id ->
            -- Note that we're waiting to get the response from the server
            -- before we subscribe to the Pusher events.
            case getParticipant id model of
                NotAsked ->
                    let
                        ( updatedModel, updatedCmds ) =
                            fetchParticipantFromBackend backendUrl accessToken id model
                    in
                        ( updatedModel
                        , updatedCmds
                        , Nothing
                        )

                Loading ->
                    ( model, Cmd.none, Nothing )

                Failure _ ->
                    let
                        ( val, cmds ) =
                            fetchParticipantFromBackend backendUrl accessToken id model
                    in
                        ( val
                        , cmds
                        , Nothing
                        )

                Success _ ->
                    ( model, Cmd.none, Nothing )

        Unsubscribe id ->
            ( { model | participants = Dict.remove id model.participants }
            , Cmd.none
            , Nothing
            )

        FetchAll ->
            let
                ( val, cmds ) =
                    fetchAllParticipantsFromBackend backendUrl accessToken model
            in
                ( val, cmds, Nothing )

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
                ( subModel, subCmd, redirectPage ) =
                    Pages.Activity.Update.update backendUrl accessToken user subMsg model.activityPage
            in
                ( { model | activityPage = subModel }
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
                                    |> Maybe.withDefault Pages.Participant.Model.emptyModel

                            ( participantUpdated, subModel, subCmd, redirectPage ) =
                                Pages.Participant.Update.update currentDate backendUrl accessToken user language subMsg ( id, participant ) participantModel
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

        HandleFetchedParticipants (Ok participants) ->
            ( { model | participants = wrapParticipantsDict participants }
            , Cmd.none
            , Nothing
            )

        HandleFetchedParticipants (Err err) ->
            let
                _ =
                    Debug.log "HandleFetchedParticipants" err
            in
                ( model, Cmd.none, Nothing )

        HandleFetchedParticipant participantId (Ok participant) ->
            let
                -- Let Participant settings fetch own data.
                -- @todo: Pass the activePage here, so we can fetch
                -- data only when really needed.
                updatedModel =
                    { model | participants = Dict.insert participantId (Success participant) model.participants }
            in
                case participant.info of
                    ParticipantChild child ->
                        -- Lazy load the Mother if they are already connected.
                        Maybe.map
                            (\motherId ->
                                update currentDate backendUrl accessToken user language (Subscribe motherId) updatedModel
                            )
                            child.motherId
                            |> Maybe.withDefault ( updatedModel, Cmd.none, Nothing )

                    ParticipantMother mother ->
                        let
                            -- Lazy load the Children, by iterating over the
                            -- children IDs and fetching them.
                            ( modelUpdatedAfterFetchingChildren, cmdsAfterFetchingChildren ) =
                                List.foldl
                                    (\childId accum ->
                                        let
                                            model =
                                                Tuple.first accum

                                            cmds =
                                                Tuple.second accum

                                            ( newModel, newCmds, _ ) =
                                                update currentDate backendUrl accessToken user language (Subscribe childId) model
                                        in
                                            ( newModel, [ newCmds ] ++ cmds )
                                    )
                                    ( updatedModel, [] )
                                    mother.children
                        in
                            ( modelUpdatedAfterFetchingChildren
                            , Cmd.batch cmdsAfterFetchingChildren
                            , Nothing
                            )

        HandleFetchedParticipant participantId (Err err) ->
            ( { model | participants = Dict.insert participantId (Failure err) model.participants }
            , Cmd.none
            , Nothing
            )

        HandlePusherEvent result ->
            case result of
                Ok event ->
                    case event.data of
                        ParticipantUpdate data ->
                            -- For now we just update the participant in the participants Dict. But this
                            -- can be used to pipe the pushed data to child components.
                            ( { model | participants = Dict.insert event.participantId (Success data) model.participants }
                            , Cmd.none
                            , Nothing
                            )

                Err err ->
                    let
                        _ =
                            Debug.log "Pusher decode Err" err
                    in
                        -- We'll log the error decoding the pusher event
                        ( model, Cmd.none, Nothing )

        SetActivityTypeFilters activityTypeFilters ->
            update currentDate backendUrl accessToken user language (MsgPagesParticipants <| Pages.Participants.Model.SetActivityTypeFilters activityTypeFilters) model


{-| A single port for all messages coming in from pusher for a `Participant` ... they
will flow in once `subscribeParticipant` is called. We'll wrap the structures on
the Javascript side so that we can dispatch them from here.
-}
port pusherParticipantMessages : (Value -> msg) -> Sub msg


fetchParticipantFromBackend : BackendUrl -> String -> ParticipantId -> Model -> ( Model, Cmd Msg )
fetchParticipantFromBackend backendUrl accessToken participantId model =
    let
        command =
            -- @todo: We need to know which activity type it is, in order to
            -- call to correct RESTful resource.
            HttpBuilder.get (backendUrl ++ "/api/patients/" ++ (toString participantId))
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> sendWithHandler decodeParticipantFromResponse (HandleFetchedParticipant participantId)
    in
        ( { model | participants = Dict.insert participantId Loading model.participants }
        , command
        )


fetchAllParticipantsFromBackend : BackendUrl -> String -> Model -> ( Model, Cmd Msg )
fetchAllParticipantsFromBackend backendUrl accessToken model =
    let
        command =
            HttpBuilder.get (backendUrl ++ "/api/patients")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> sendWithHandler decodeParticipantsFromResponse HandleFetchedParticipants
    in
        ( model
        , command
        )


subscriptions : Model -> Page -> Sub Msg
subscriptions model activePage =
    let
        pageSubs =
            case activePage of
                Participant participantId ->
                    case Dict.get participantId model.participantPage of
                        Just participantPage ->
                            [ Sub.map (MsgPagesParticipant participantId) (Pages.Participant.Update.subscriptions participantPage) ]

                        Nothing ->
                            []

                _ ->
                    []
    in
        Sub.batch
            ([ pusherParticipantMessages (decodeValue decodePusherEvent >> HandlePusherEvent) ] ++ pageSubs)
