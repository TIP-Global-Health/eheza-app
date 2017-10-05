port module App.Update exposing (init, update, subscriptions)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.Model exposing (..)
import App.PageType exposing (Page(..))
import Backend.Entities exposing (ClinicId, SessionId)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.Session.Model exposing (Session)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Config
import Date
import Dict
import Drupal.Restful exposing (EndPoint, toNodeId)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Http exposing (Error)
import FilePicker.Model
import Maybe.Extra
import Pages.Activity.Model
import Pages.Participant.Model
import ParticipantManager.Model
import ParticipantManager.Update
import Pusher.Model
import Pusher.Utils exposing (getClusterName)
import Json.Decode exposing (decodeValue, bool)
import Json.Encode exposing (Value)
import Pages.Login.Update
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Time exposing (minute)
import Update.Extra exposing (sequence)
import User.Model exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        user =
            if (String.isEmpty flags.accessToken) then
                -- This isn't really a netowrk error, but we mark the user as
                -- Failure, so we know we have an anonymous user at hand.
                Failure <| Http.NetworkError
            else
                NotAsked

        ( config, cmds, activePage ) =
            case (Dict.get flags.hostname Config.configs) of
                Just config ->
                    let
                        defaultCmds =
                            [ pusherKey
                                ( config.pusherKey.key
                                , getClusterName config.pusherKey.cluster
                                , Pusher.Model.eventNames
                                )
                            , Task.perform SetCurrentDate Date.now
                            ]

                        ( cmds, activePage_ ) =
                            if (String.isEmpty flags.accessToken) then
                                -- Check if we have already an access token.
                                ( defaultCmds, Login )
                            else
                                ( [ Cmd.map PageLogin <| Pages.Login.Update.fetchUserFromBackend config.backendUrl flags.accessToken ] ++ defaultCmds
                                , emptyModel.activePage
                                )
                    in
                        ( Success config
                        , cmds
                        , activePage_
                        )

                Nothing ->
                    ( Failure "No config found"
                    , [ Cmd.none ]
                    , emptyModel.activePage
                    )
    in
        ( { emptyModel
            | accessToken = flags.accessToken
            , activePage = activePage
            , config = config
            , user = user
          }
        , Cmd.batch cmds
        )


clinicEndpoint : EndPoint Error () ClinicId Clinic
clinicEndpoint =
    { path = "api/clinics"
    , tag = toNodeId
    , decoder = decodeClinic
    , error = identity
    , params = always []
    }


{-| Type-safe params ... how nice!
-}
type alias SessionParams =
    { openOn : Maybe NominalDate
    }


encodeSessionParams : SessionParams -> List ( String, String )
encodeSessionParams params =
    params.openOn
        |> Maybe.map (\open -> ( "open_on", Gizra.NominalDate.formatYYYYMMDD open ))
        |> Maybe.Extra.toList


sessionEndpoint : EndPoint Error SessionParams SessionId Session
sessionEndpoint =
    { path = "api/sessions"
    , tag = toNodeId
    , decoder = decodeSession
    , error = identity
    , params = encodeSessionParams
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        backendUrl =
            case model.config of
                Success config ->
                    config.backendUrl

                _ ->
                    ""

        getFromBackend =
            -- Partially apply the backendUrl and accessToken, just for fun
            Drupal.Restful.get backendUrl (Just model.accessToken)
    in
        case msg of
            FetchClinics ->
                -- Ultimately, it would be nice to preserve any existing value of clnics
                -- if we're reloading ... will need an `UpdateableWebData` for that.
                ( { model | clinics = Loading }
                , getFromBackend clinicEndpoint () <|
                    (RemoteData.fromResult >> RemoteData.map EveryDictList.fromList >> HandleFetchedClinics)
                )

            HandleFetchedClinics clinics ->
                ( { model | clinics = clinics }
                , Cmd.none
                )

            FetchSessionsOpenOn date ->
                ( { model | openSessions = Loading }
                , getFromBackend sessionEndpoint (SessionParams (Just date)) <|
                    (RemoteData.fromResult >> RemoteData.map EveryDictList.fromList >> HandleFetchedSessions date)
                )

            HandleFetchedSessions date result ->
                -- We remember the date as well as the result, so that we can
                -- know whether we need to reload (i.e. when the date changes,
                -- due to the passage of time)
                ( { model | openSessions = RemoteData.map (\sessions -> ( date, sessions )) result }
                , Cmd.none
                )

            HandleOfflineEvent (Ok offline) ->
                { model | offline = offline } ! []

            HandleOfflineEvent (Err err) ->
                model ! []

            Logout ->
                ( { emptyModel
                    | accessToken = ""
                    , activePage = Login
                    , config = model.config
                  }
                , accessTokenPort ""
                )

            MsgParticipantManager subMsg ->
                case model.user of
                    Success user ->
                        let
                            ( val, cmds, redirectPage ) =
                                ParticipantManager.Update.update model.currentDate backendUrl model.accessToken user model.language subMsg model.pageParticipant

                            modelUpdated =
                                { model | pageParticipant = val }

                            ( modelUpdatedWithSetPage, setPageCmds ) =
                                Maybe.map
                                    (\page ->
                                        update (SetActivePage page) modelUpdated
                                    )
                                    redirectPage
                                    |> Maybe.withDefault ( modelUpdated, Cmd.none )
                        in
                            ( modelUpdatedWithSetPage
                            , Cmd.batch
                                [ Cmd.map MsgParticipantManager cmds
                                , setPageCmds
                                ]
                            )

                    _ ->
                        -- If we don't have a user, we have nothing to do.
                        model ! []

            PageLogin msg ->
                let
                    ( val, cmds, ( webDataUser, accessToken ) ) =
                        Pages.Login.Update.update backendUrl msg model.pageLogin

                    modelUpdated =
                        { model
                            | pageLogin = val
                            , accessToken = accessToken
                            , user = webDataUser
                        }

                    ( modelWithRedirect, setActivePageCmds ) =
                        case webDataUser of
                            -- If user was successfuly fetched, reditect to my
                            -- account page.
                            Success _ ->
                                let
                                    nextPage =
                                        case modelUpdated.activePage of
                                            Login ->
                                                -- Redirect to the dashboard.
                                                Dashboard []

                                            _ ->
                                                -- Keep the active page.
                                                modelUpdated.activePage
                                in
                                    update (SetActivePage nextPage) modelUpdated

                            Failure _ ->
                                -- Unset the wrong access token.
                                update (SetActivePage Login) { modelUpdated | accessToken = "" }

                            _ ->
                                modelUpdated ! []
                in
                    ( modelWithRedirect
                    , Cmd.batch
                        [ Cmd.map PageLogin cmds
                        , accessTokenPort accessToken
                        , setActivePageCmds
                        ]
                    )

            RedirectByActivePage ->
                update (SetActivePage <| getBackButtonTarget model.activePage) model

            SetActivePage page ->
                let
                    activePageUpdated =
                        setActivePageAccess model.user page

                    unbindFilePickerMsg =
                        case model.activePage of
                            Activity (Just (Child ChildPicture)) ->
                                [ MsgParticipantManager <|
                                    ParticipantManager.Model.MsgPagesActivity <|
                                        Pages.Activity.Model.MsgFilePicker FilePicker.Model.Unbind
                                ]

                            Participant participantId ->
                                [ MsgParticipantManager <|
                                    ParticipantManager.Model.MsgPagesParticipant participantId <|
                                        Pages.Participant.Model.MsgFilePicker FilePicker.Model.Unbind
                                ]

                            _ ->
                                []

                    ( modelUpdated, command ) =
                        -- For a few, we also delegate some initialization
                        case activePageUpdated of
                            Activities ->
                                -- If we're showing a `Activities` page, make sure we `Subscribe`
                                update (MsgParticipantManager ParticipantManager.Model.FetchAll) model

                            Activity maybeActivityType ->
                                let
                                    currentActivityPage =
                                        model.pageParticipant.activityPage

                                    updatedActivityPage =
                                        case maybeActivityType of
                                            Just activityType ->
                                                let
                                                    isActive =
                                                        case activityType of
                                                            Child ChildPicture ->
                                                                True

                                                            _ ->
                                                                False
                                                in
                                                    { currentActivityPage | selectedActivity = activityType }

                                            _ ->
                                                currentActivityPage

                                    currentParticipanstManagerPage =
                                        model.pageParticipant

                                    updatedParticipanstManagerPage =
                                        { currentParticipanstManagerPage | activityPage = updatedActivityPage }
                                in
                                    -- If we're showing a `Activities` page, make sure we `Subscribe`
                                    update (MsgParticipantManager ParticipantManager.Model.FetchAll)
                                        { model | pageParticipant = updatedParticipanstManagerPage }

                            Dashboard activityTypes ->
                                update (MsgParticipantManager ParticipantManager.Model.FetchAll) model

                            Participant id ->
                                -- If we're showing a `Participant`, make sure we `Subscribe`
                                update (MsgParticipantManager (ParticipantManager.Model.Subscribe id)) model

                            _ ->
                                ( model, Cmd.none )
                in
                    sequence update
                        unbindFilePickerMsg
                        ( { modelUpdated | activePage = setActivePageAccess model.user activePageUpdated }
                        , Cmd.batch
                            [ activePage [ (toString activePageUpdated), backendUrl ]
                            , command
                            ]
                        )

            SetCurrentDate date ->
                { model | currentDate = date } ! []

            ThemeSwitch currentTheme ->
                let
                    newTheme =
                        case currentTheme of
                            Dark ->
                                Light

                            Light ->
                                Dark

                    config =
                        { from = String.toLower <| toString <| currentTheme
                        , to = String.toLower <| toString <| newTheme
                        }
                in
                    ( { model | theme = newTheme }
                    , themeSwitcher config
                    )

            Tick _ ->
                model ! [ Task.perform SetCurrentDate Date.now ]


{-| Determine the target page of the back button based on the active page.
-}
getBackButtonTarget : Page -> Page
getBackButtonTarget activePage =
    case activePage of
        AccessDenied ->
            activePage

        Activities ->
            Dashboard []

        Activity _ ->
            Activities

        Dashboard activity ->
            Activities

        Login ->
            activePage

        MyAccount ->
            activePage

        OpenSessions ->
            activePage

        PageNotFound ->
            activePage

        Participant participantId ->
            Dashboard []


{-| Determine is a page can be accessed by a user (anonymous or authenticated),
and if not return a access denied page.

If the user is authenticated, don't allow them to revisit Login page. Do the
opposite for anonymous user - don't allow them to visit the MyAccount page.

-}
setActivePageAccess : WebData User -> Page -> Page
setActivePageAccess user page =
    case user of
        Success _ ->
            if page == Login then
                AccessDenied
            else
                page

        Failure _ ->
            if page == Login then
                page
            else if page == PageNotFound then
                page
            else
                AccessDenied

        _ ->
            page


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MsgParticipantManager <| ParticipantManager.Update.subscriptions model.pageParticipant model.activePage
        , Time.every minute Tick
        , offline (decodeValue bool >> HandleOfflineEvent)
        ]


{-| Send access token to JS.
-}
port accessTokenPort : String -> Cmd msg


{-| Send Pusher key and cluster to JS.
-}
port pusherKey : ( String, String, List String ) -> Cmd msg


{-| Get a singal if internet connection is lost.
-}
port offline : (Value -> msg) -> Sub msg


{-| Send active page to JS.
-}
port activePage : List String -> Cmd msg


{-| Send the new theme configurations to JS.
-}
port themeSwitcher : ThemeConfig -> Cmd msg
