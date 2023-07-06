module Backend.Nurse.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Nurse.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (encodeEntityUuid, toCmd, withoutDecoder)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update currentDate msg model =
    case msg of
        UpdateNurse nurseId nurse ->
            updateNurse currentDate nurseId nurse model

        HandleUpdatedNurse data ->
            ( { model | updateNurse = data }
            , Cmd.none
            )


updateNurse : NominalDate -> NurseId -> Nurse -> Model -> ( Model, Cmd Msg )
updateNurse currentDate nurseId nurse model =
    ( { model | updateNurse = Loading }
    , sw.patchFull nurseEndpoint nurseId nurse
        |> withoutDecoder
        |> toCmd (RemoteData.fromResult >> HandleUpdatedNurse)
    )