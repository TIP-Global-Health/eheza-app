module Backend.Nurse.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (nurseEndpoint)
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Model, Msg(..), Nurse)
import Backend.Utils exposing (sw)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        UpdateNurse nurseId nurse ->
            updateNurse nurseId nurse model

        HandleUpdatedNurse data ->
            ( { model | updateNurse = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateNurse : NurseId -> Nurse -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
updateNurse nurseId nurse model =
    ( { model | updateNurse = Loading }
    , sw.patchFull nurseEndpoint nurseId nurse
        |> withoutDecoder
        |> toCmd (RemoteData.fromResult >> HandleUpdatedNurse)
    , []
    )
