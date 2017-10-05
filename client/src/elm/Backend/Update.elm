module Backend.Update exposing (update)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeSession)
import Backend.Session.Model exposing (Session)
import Config.Model exposing (BackendUrl)
import Drupal.Restful exposing (EndPoint, toNodeId)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Maybe.Extra exposing (toList)
import RemoteData exposing (RemoteData(..))


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


update : BackendUrl -> String -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl accessToken msg model =
    let
        getFromBackend =
            -- Partially apply the backendUrl and accessToken, just for fun
            Drupal.Restful.get backendUrl (Just accessToken)
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
