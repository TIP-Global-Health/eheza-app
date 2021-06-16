module Backend.WellChildEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Backend.WellChildEncounter.Encoder exposing (encodeWellChildEncounter)
import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> WellChildEncounterId -> Maybe WellChildEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseWellChildEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeWellChildEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull homeVisitEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedWellChildEncounter)
                        )
                    )

        HandleClosedWellChildEncounter data ->
            ( { model | closeWellChildEncounter = data }
            , Cmd.none
            )
