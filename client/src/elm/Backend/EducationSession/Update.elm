module Backend.EducationSession.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.EducationSession.Model exposing (..)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
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
        Close ->
            maybeSession
                |> unwrap ( model, Cmd.none, [] )
                    (\session ->
                        update currentDate sessionId maybeSession (Update { session | endDate = Just currentDate }) model
                    )

        Update session ->
            ( { model | updateEducationSession = Loading }
            , sw.patchFull educationSessionEndpoint sessionId session
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleUpdated)
            , []
            )

        HandleUpdated data ->
            ( { model | updateEducationSession = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
