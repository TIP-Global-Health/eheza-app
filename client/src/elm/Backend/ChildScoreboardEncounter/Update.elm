module Backend.ChildScoreboardEncounter.Update exposing (update)

import Backend.ChildScoreboardEncounter.Encoder exposing (encodeChildScoreboardEncounter)
import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> ChildScoreboardEncounterId -> Maybe ChildScoreboardEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseChildScoreboardEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeChildScoreboardEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull childScoreboardEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedChildScoreboardEncounter)
                        )
                    )

        HandleClosedChildScoreboardEncounter data ->
            ( { model | closeChildScoreboardEncounter = data }
            , Cmd.none
            )
