module Pages.StockManagement.Update exposing (update)

import App.Model
import App.Ports exposing (bindSignaturePad, clearSignaturePad, storeSignature)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ImageUrl(..), StockUpdateType(..))
import Backend.Model
import Backend.StockUpdate.Model
import Backend.StockUpdate.Utils exposing (stockSupplierFromString)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra
import Pages.StockManagement.Model exposing (CorrectionEntryType(..), DisplayMode(..), Model, Msg(..), emptyModel)
import Pages.StockManagement.Utils exposing (correctionEntryTypeFromString)


update : NominalDate -> Maybe HealthCenterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate maybeHealthCenterId msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDisplayMode mode ->
            let
                signaturePadCmd =
                    case mode of
                        ModeReceiveStock ->
                            if model.receiveStockForm.confirmIdentity == Just True then
                                bindSignaturePad ()

                            else
                                Cmd.none

                        ModeCorrectEntry ->
                            if model.correctEntryForm.confirmIdentity == Just True then
                                bindSignaturePad ()

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | displayMode = mode }
            , signaturePadCmd
            , []
            )

        StoreSignature ->
            ( model
            , storeSignature ()
            , []
            )

        ClearSignaturePad ->
            ( model
            , clearSignaturePad ()
            , []
            )

        ChangeMonthGap value ->
            ( { model | monthGap = model.monthGap + value }
            , Cmd.none
            , []
            )

        ChangeDetailsMonthGap value ->
            case model.displayMode of
                ModeMonthDetails monthGap ->
                    ( model
                    , Cmd.none
                    , []
                    )
                        |> sequenceExtra (update currentDate maybeHealthCenterId)
                            [ SetDisplayMode (ModeMonthDetails (monthGap + value)) ]

                _ ->
                    ( model
                    , Cmd.none
                    , []
                    )

        SetReceiveStockConfirmIdentity confirmed ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | confirmIdentity = Just confirmed, displayIdentityPopup = not confirmed }

                cmd =
                    if confirmed then
                        bindSignaturePad ()

                    else
                        Cmd.none
            in
            ( { model | receiveStockForm = updatedForm }
            , cmd
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

        ReceiveStockHandleStoredSignature url ->
            let
                form =
                    model.receiveStockForm

                updatedForm =
                    { form | signature = Just <| ImageUrl url }
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
                    Maybe.map4
                        (\healthCenterId dateRecorded quantity signature ->
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
                                    , deleted = False
                                    , shard = Just healthCenterId
                                    , signature = signature
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
                        form.signature
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

                cmd =
                    if confirmed then
                        bindSignaturePad ()

                    else
                        Cmd.none
            in
            ( { model | correctEntryForm = updatedForm }
            , cmd
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

        SetCorrectionEntryType value ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | entryType = correctionEntryTypeFromString value }
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

        CorrectEntryHandleStoredSignature url ->
            let
                form =
                    model.correctEntryForm

                updatedForm =
                    { form | signature = Just <| ImageUrl url }
            in
            ( { model | correctEntryForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveCorrectEntry nurseId ->
            let
                form =
                    model.correctEntryForm

                ( action, updatedModel ) =
                    Maybe.map5
                        (\healthCenterId dateRecorded quantity entryType signature ->
                            let
                                record =
                                    { nurse = nurseId
                                    , dateMeasured = currentDate
                                    , updateType = UpdateCorrection
                                    , quantity =
                                        case entryType of
                                            EntryAddition ->
                                                quantity

                                            EntrySubstraction ->
                                                -1 * quantity
                                    , dateRecorded = dateRecorded
                                    , dateExpires = Nothing
                                    , batchNumber = Nothing
                                    , supplier = Nothing
                                    , notes = Nothing
                                    , correctionReason = form.reason
                                    , healthCenter = healthCenterId
                                    , deleted = False
                                    , shard = Just healthCenterId
                                    , signature = signature
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
                        form.date
                        form.quantity
                        form.entryType
                        form.signature
                        |> Maybe.withDefault ( [], model )
            in
            ( updatedModel
            , Cmd.none
            , action
            )
