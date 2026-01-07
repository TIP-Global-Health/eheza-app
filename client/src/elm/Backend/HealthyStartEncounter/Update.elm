module Backend.HealthyStartEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.HealthyStartEncounter.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> HealthyStartEncounterId
    -> Maybe HealthyStartEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseHealthyStartEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeHealthyStartEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull healthyStartEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedHealthyStartEncounter)
                        , []
                        )
                    )

        HandleClosedHealthyStartEncounter data ->
            ( { model | closeHealthyStartEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
