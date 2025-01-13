module Backend.EducationSession.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.EducationSession.Model exposing (..)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> EducationSessionId
    -> Maybe EducationSession
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate sessionId maybeSession msg model =
    case msg of
        Update updateFunc ->
            unwrap ( model, Cmd.none, [] )
                (\session ->
                    ( { model | updateEducationSession = Loading }
                    , updateFunc session
                        |> sw.patchFull educationSessionEndpoint sessionId
                        |> withoutDecoder
                        |> toCmd (RemoteData.fromResult >> HandleUpdated)
                    , []
                    )
                )
                maybeSession

        HandleUpdated data ->
            ( { model | updateEducationSession = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
