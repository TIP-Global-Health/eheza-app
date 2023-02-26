module Pages.StockManagement.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model
import Backend.StockUpdate.Model exposing (StockUpdateType(..))
import Backend.StockUpdate.Utils exposing (stockSupplierFromString)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.StockManagement.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Time
import Time.Extra


update : NominalDate -> Maybe HealthCenterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate maybeHealthCenterId msg model =
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
                    { form | dateRecordedSelectorPopupState = state, dateRecorded = defaultSelection }
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
                    { form | batchNumber = Just value }
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
                    { form | dateExpiresSelectorPopupState = state, dateExpires = defaultSelection }
            in
            ( { model | receiveStockForm = updatedForm }
            , Cmd.none
            , []
            )

        SetNotes value ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | notes = Just value }
            in
            ( { model | receiveStockForm = updatedForm }
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

        SaveReceiveStock nurseId ->
            let
                form =
                    model.receiveStockForm

                ( action, updatedModel ) =
                    Maybe.map3
                        (\healthCenterId dateRecorded quantity ->
                            let
                                record =
                                    { nurse = nurseId
                                    , dateMeasured = currentDate
                                    , updateType = UpdateReceivingSupplies
                                    , quantity = quantity
                                    , dateRecorded = dateRecorded
                                    , dateExpires = form.dateExpires
                                    , batchNumber = form.batchNumber
                                    , supplier = form.supplier
                                    , notes = form.notes
                                    , correctionReason = Nothing
                                    , healthCenter = healthCenterId
                                    , shard = Just healthCenterId
                                    , signature = Nothing
                                    }
                            in
                            ( [ Backend.StockUpdate.Model.CreateStockUpdate record
                                    |> Backend.Model.MsgStockUpdate nurseId
                                    |> App.Model.MsgIndexedDb
                              ]
                            , emptyModel
                            )
                        )
                        maybeHealthCenterId
                        form.dateRecorded
                        form.quantity
                        |> Maybe.withDefault ( [], model )
            in
            ( updatedModel
            , Cmd.none
            , action
            )

        SetCorrectEntryConfirmIdentity confirmed ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | confirmIdentity = Just confirmed, displayIdentityPopup = not confirmed }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDate value ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | date = Just value }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDateSelectorState state ->
            let
                form =
                    model.correctEntryForm

                defaultSelection =
                    Maybe.Extra.or form.date (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, date = defaultSelection }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetQuantityDeducted value ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | quantity = String.toInt value }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetCorrectionReason value ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | reason = Just value }
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
            ( emptyModel
            , Cmd.none
            , []
            )
