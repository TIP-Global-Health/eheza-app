port module PatientManager.Update exposing (update, subscriptions, dashboardUrlFragment)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import HttpBuilder exposing (get, withJsonBody, withQueryParams)
import Pages.Activities.Update
import Pages.Patient.Model
import Pages.Patient.Update
import Pages.Patients.Model
import Pages.Patients.Update
import Patient.Model exposing (Patient, PatientId, PatientType(..))
import PatientManager.Decoder
    exposing
        ( decodePatientFromResponse
        , decodePatientsFromResponse
        )
import PatientManager.Model exposing (..)
import PatientManager.Utils exposing (..)
import Pusher.Decoder exposing (decodePusherEvent)
import Pusher.Model exposing (PusherEventData(..))
import RemoteData exposing (RemoteData(..))
import User.Model exposing (User)
import Utils.WebData exposing (sendWithHandler)


{-| If we're on the `Dashboard` page, what should we show in the part of the URL
that we'll be asked to decode?
-}
dashboardUrlFragment : Model -> String
dashboardUrlFragment model =
    Pages.Patients.Update.urlFragment model.patientsPage


update : Date -> BackendUrl -> String -> User -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update currentDate backendUrl accessToken user msg model =
    case msg of
        Subscribe id ->
            -- Note that we're waiting to get the response from the server
            -- before we subscribe to the Pusher events.
            case getPatient id model of
                NotAsked ->
                    let
                        ( updatedModel, updatedCmds ) =
                            fetchPatientFromBackend backendUrl accessToken id model
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
                            fetchPatientFromBackend backendUrl accessToken id model
                    in
                        ( val
                        , cmds
                        , Nothing
                        )

                Success _ ->
                    ( model, Cmd.none, Nothing )

        Unsubscribe id ->
            ( { model | patients = Dict.remove id model.patients }
            , Cmd.none
            , Nothing
            )

        FetchAll ->
            let
                ( val, cmds ) =
                    fetchAllPatientsFromBackend backendUrl accessToken model
            in
                ( val, cmds, Nothing )

        MsgPagesActivities subMsg ->
            let
                ( subModel, subCmd, redirectPage ) =
                    Pages.Activities.Update.update backendUrl accessToken user subMsg (unwrapPatientsDict model.patients) model.activitiesPage
            in
                ( { model | activitiesPage = subModel }
                , Cmd.map MsgPagesActivities subCmd
                , redirectPage
                )

        MsgPagesPatient id subMsg ->
            let
                patientEntry =
                    getPatient id model
            in
                case patientEntry of
                    Success patient ->
                        let
                            patientModel =
                                Maybe.map identity (Dict.get id model.patientPage)
                                    |> Maybe.withDefault Pages.Patient.Model.emptyModel

                            ( patientUpdated, subModel, subCmd, redirectPage ) =
                                Pages.Patient.Update.update backendUrl accessToken user subMsg ( id, patient ) patientModel
                        in
                            ( { model
                                | patients = Dict.insert id (Success patientUpdated) model.patients
                                , patientPage = Dict.insert id subModel model.patientPage
                              }
                            , Cmd.map (MsgPagesPatient id) subCmd
                            , redirectPage
                            )

                    _ ->
                        -- We've received a message for a Patient which we either
                        -- aren't subscribed to, or dont' have initial data for yet.
                        -- This normally wouldn't happen, though we may needd to think
                        -- about synchronization between obtaining our initial data and
                        -- possible "pusher" messages. (Could pusher messages sometimes
                        -- arrive before the initial data, and if so, should we ignore
                        -- them or queue them up? We may need server timestamps on the initial
                        -- data and the pusher messages to know.)
                        ( model, Cmd.none, Nothing )

        MsgPagesPatients subMsg ->
            let
                ( subModel, subCmd, redirectPage ) =
                    Pages.Patients.Update.update backendUrl accessToken user subMsg (unwrapPatientsDict model.patients) model.patientsPage
            in
                ( { model | patientsPage = subModel }
                , Cmd.map MsgPagesPatients subCmd
                , redirectPage
                )

        HandleFetchedPatients (Ok patients) ->
            ( { model | patients = wrapPatientsDict patients }
            , Cmd.none
            , Nothing
            )

        HandleFetchedPatients (Err err) ->
            let
                _ =
                    Debug.log "HandleFetchedPatients" err
            in
                ( model, Cmd.none, Nothing )

        HandleFetchedPatient patientId (Ok patient) ->
            let
                -- Let Patient settings fetch own data.
                -- @todo: Pass the activePage here, so we can fetch
                -- data only when really needed.
                updatedModel =
                    { model | patients = Dict.insert patientId (Success patient) model.patients }
            in
                case patient.info of
                    PatientChild child ->
                        -- Lazy load the Mother if they are already connected.
                        Maybe.map
                            (\motherId ->
                                update currentDate backendUrl accessToken user (Subscribe motherId) updatedModel
                            )
                            child.motherId
                            |> Maybe.withDefault ( updatedModel, Cmd.none, Nothing )

                    PatientMother mother ->
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
                                                update currentDate backendUrl accessToken user (Subscribe childId) model
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

        HandleFetchedPatient patientId (Err err) ->
            ( { model | patients = Dict.insert patientId (Failure err) model.patients }
            , Cmd.none
            , Nothing
            )

        HandlePusherEvent result ->
            case result of
                Ok event ->
                    case event.data of
                        PatientUpdate data ->
                            -- For now we just update the patient in the patients Dict. But this
                            -- can be used to pipe the pushed data to child components.
                            ( { model | patients = Dict.insert event.patientId (Success data) model.patients }
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
            update currentDate backendUrl accessToken user (MsgPagesPatients <| Pages.Patients.Model.SetActivityTypeFilters activityTypeFilters) model


{-| A single port for all messages coming in from pusher for a `Patient` ... they
will flow in once `subscribePatient` is called. We'll wrap the structures on
the Javascript side so that we can dispatch them from here.
-}
port pusherPatientMessages : (Value -> msg) -> Sub msg


fetchPatientFromBackend : BackendUrl -> String -> PatientId -> Model -> ( Model, Cmd Msg )
fetchPatientFromBackend backendUrl accessToken patientId model =
    let
        command =
            -- @todo: We need to know which activity type it is, in order to
            -- call to correct RESTful resource.
            HttpBuilder.get (backendUrl ++ "/api/patients/" ++ (toString patientId))
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> sendWithHandler decodePatientFromResponse (HandleFetchedPatient patientId)
    in
        ( { model | patients = Dict.insert patientId Loading model.patients }
        , command
        )


fetchAllPatientsFromBackend : BackendUrl -> String -> Model -> ( Model, Cmd Msg )
fetchAllPatientsFromBackend backendUrl accessToken model =
    let
        command =
            HttpBuilder.get (backendUrl ++ "/api/patients")
                |> withQueryParams [ ( "access_token", accessToken ) ]
                |> sendWithHandler decodePatientsFromResponse HandleFetchedPatients
    in
        ( model
        , command
        )


subscriptions : Model -> Page -> Sub Msg
subscriptions model activePage =
    pusherPatientMessages (decodeValue decodePusherEvent >> HandlePusherEvent)
