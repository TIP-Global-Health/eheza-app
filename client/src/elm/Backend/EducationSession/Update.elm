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
update currentDate sessionId maybeEncounter msg model =
    case msg of
        CloseEducationSession ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | updateEducationSession = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull educationSessionEndpoint sessionId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedEducationSession)
                        , []
                        )
                    )

        HandleClosedEducationSession data ->
            ( { model | updateEducationSession = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
