module Backend.TuberculosisEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.TuberculosisEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> TuberculosisEncounterId
    -> Maybe TuberculosisEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseTuberculosisEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeTuberculosisEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull tuberculosisEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedTuberculosisEncounter)
                        , []
                        )
                    )

        HandleClosedTuberculosisEncounter data ->
            ( { model | closeTuberculosisEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
