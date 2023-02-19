module Pages.StockManagement.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model
import Backend.StockUpdate.Utils exposing (stockSupplierFromString)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
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

        SetReceiveStockConfirmIdentity confirmed ->
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

        SetDateRecorded value ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | dateRecorded = Just value }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDateRecordedSelectorState state ->
            let
                form =
                    model.receiveStockForm

                defaultSelection =
                    Maybe.Extra.or form.dateRecorded (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, dateRecorded = defaultSelection }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetStockSupplier value ->
            let
                form =
                    model.receiveStockForm

                supplier =
                    stockSupplierFromString value

                updatedForm =
                    { form | supplier = supplier }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBatchNumber value ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | batchNumber = value }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetQuantityAdded value ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | quantity = String.toInt value }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDateExpires value ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | dateExpires = Just value }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDateExpiresSelectorState state ->
            let
                form =
                    model.receiveStockForm

                defaultSelection =
                    Maybe.Extra.or form.dateExpires (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, dateExpires = defaultSelection }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveReceiveStock ->
            -- @todo
            ( model
            , Cmd.none
            , []
            )

        HideReceiveStockIdentityPopup ->
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

        SetCorrectEntryConfirmIdentity confirmed ->
            let
                _ =
                    Debug.log "confirmed" confirmed

                form =
                    model.correctEntryForm

                updatedForm =
                    { form | confirmIdentity = Just confirmed, displayIdentityPopup = not confirmed }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        HideCorrectEntryIdentityPopup ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | displayIdentityPopup = False }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveCorrectEntry ->
            -- @todo
            ( model
            , Cmd.none
            , []
            )
