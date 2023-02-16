module Pages.StockManagement.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Pages.StockManagement.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Time
import Time.Extra


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDisplayMode mode ->
            ( { model | displayMode = mode }
            , Cmd.none
            , []
            )

        SetConfirmIdentity confirmed ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | confirmIdentity = Just confirmed, displayIdentityPopup = not confirmed }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        HideIdentityPopup ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | displayIdentityPopup = False }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )
