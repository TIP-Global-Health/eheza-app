module Pages.StockManagement.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( ImageUrl(..)
        , StockCorrectionReason(..)
        , StockSupplier(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.StockUpdate.Model exposing (StockManagementData)
import Backend.StockUpdate.Utils exposing (dateToMonthYear, monthYearDiff, stockSupplierToString)
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYY, formatDDMMYYYY)
import Gizra.TimePosix exposing (viewTimePosix)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import List.Extra
import List.Zipper
import Maybe.Extra
import Pages.Dashboard.View exposing (chwCard)
import Pages.Page exposing (Page(..))
import Pages.StockManagement.Model exposing (CorrectEntryForm, CorrectionEntryType(..), DisplayMode(..), Model, Msg(..), ReceiveStockForm, StockManagementMenu(..), maxMonthGap)
import Pages.StockManagement.Utils exposing (correctionEntryTypeToString)
import Pages.Utils
    exposing
        ( customPopup
        , maybeToBoolTask
        , resolveSelectedDateForMonthSelector
        , resolveTasksCompletedFromTotal
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewLabel
        , viewMonthSelector
        , viewNumberInput
        , viewPhotoThumbFromImageUrl
        , viewQuestionLabel
        , viewSaveAction
        , viewSelectListInput
        , viewTasksCount
        , viewTextInput
        )
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Round
import SyncManager.Model exposing (SyncInfoAuthorityZipper)
import Time
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (sortByDateDesc)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> Maybe HealthCenterId
    -> NurseId
    -> Nurse
    -> SyncInfoAuthorityZipper
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate maybeHealthCenterId nurseId nurse syncInfoAuthorities db model =
    Maybe.andThen
        (\healthCenterId ->
            Dict.get healthCenterId db.stockManagementData
        )
        maybeHealthCenterId
        |> Maybe.withDefault NotAsked
        |> viewWebData language
            (viewHeaderAndContent language currentDate maybeHealthCenterId nurseId nurse syncInfoAuthorities model)
            identity


viewHeaderAndContent :
    Language
    -> NominalDate
    -> Maybe HealthCenterId
    -> NurseId
    -> Nurse
    -> SyncInfoAuthorityZipper
    -> Model
    -> StockManagementData
    -> Html Msg
viewHeaderAndContent language currentDate maybeHealthCenterId nurseId nurse syncInfoAuthorities model data =
    let
        header =
            let
                ( label, action ) =
                    case model.displayMode of
                        ModeMain ->
                            ( Translate.StockManagement, SetActivePage PinCodePage )

                        ModeMonthDetails _ ->
                            ( Translate.StockManagement, SetDisplayMode ModeMain )

                        ModeReceiveStock ->
                            ( Translate.StockManagementMenu MenuReceiveStock, SetDisplayMode ModeMain )

                        ModeCorrectEntry ->
                            ( Translate.StockManagementMenu MenuCorrectEntry, SetDisplayMode ModeMain )
            in
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ text <| translate language label ]
                , span
                    [ class "link-back"
                    , onClick action
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]

        content =
            case model.displayMode of
                ModeMain ->
                    let
                        lastUpdated =
                            viewLastUpdated language maybeHealthCenterId syncInfoAuthorities
                    in
                    viewModeMain language currentDate lastUpdated data model

                ModeMonthDetails monthGap ->
                    viewModeMonthDetails language currentDate monthGap data

                ModeReceiveStock ->
                    let
                        consumptionAverage =
                            Dict.get (dateToMonthYear currentDate) data
                                |> Maybe.map .consumptionAverage
                    in
                    viewModeReceiveStock language currentDate nurseId nurse consumptionAverage model.receiveStockForm

                ModeCorrectEntry ->
                    viewModeCorrectEntry language currentDate nurseId nurse model.correctEntryForm
    in
    div [ class "page-activity stock-management" ] <|
        header
            :: content


viewLastUpdated :
    Language
    -> Maybe HealthCenterId
    -> SyncInfoAuthorityZipper
    -> Html Msg
viewLastUpdated language maybeHealthCenterId syncInfoAuthorities =
    Maybe.map2
        (\healthCenterId syncInfoZipper ->
            List.Zipper.toList syncInfoZipper
                |> List.filter
                    (\zipper ->
                        zipper.uuid == fromEntityUuid healthCenterId
                    )
                |> List.head
                |> Maybe.map
                    (\info ->
                        let
                            dateAndTime =
                                if info.lastSuccesfulContact == 0 then
                                    translate language Translate.Never

                                else
                                    Time.millisToPosix info.lastSuccesfulContact
                                        |> viewTimePosix language
                        in
                        div [ class "timestamp" ]
                            [ text <| (translate language <| Translate.Dashboard Translate.LastUpdated) ++ ": " ++ dateAndTime ]
                    )
                |> Maybe.withDefault emptyNode
        )
        maybeHealthCenterId
        syncInfoAuthorities
        |> Maybe.withDefault emptyNode


viewModeMain :
    Language
    -> NominalDate
    -> Html Msg
    -> StockManagementData
    -> Model
    -> List (Html Msg)
viewModeMain language currentDate lastUpdated data model =
    let
        viewButton label action =
            button
                [ class "ui primary button"
                , onClick action
                ]
                [ span [ class "text" ] [ text <| translate language label ]
                , span [ class "icon-back" ] []
                ]

        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        ( selectedMonthReceived, selectedMonthIssued, selectedMonthStock ) =
            let
                selectedMonthYear =
                    dateToMonthYear dateLastDayOfSelectedMonth
            in
            Dict.get selectedMonthYear data
                |> Maybe.map
                    (\dataForMonth ->
                        ( String.fromFloat dataForMonth.received
                        , String.fromFloat dataForMonth.issued
                        , Maybe.map String.fromFloat dataForMonth.currentBalance
                            |> Maybe.withDefault ""
                        )
                    )
                |> Maybe.withDefault ( "", "", "" )

        historyHeaderRow =
            div [ class "row header" ]
                [ div [ class "cell month" ] [ text <| translate language Translate.Month ]
                , div [ class "cell starting-stock" ] [ text <| translate language Translate.StartingStock ]
                , div [ class "cell received" ] [ text <| translate language Translate.Received ]
                , div [ class "cell issued" ] [ text <| translate language Translate.Issued ]
                , div [ class "cell balance" ] [ text <| translate language Translate.Balance ]
                , div [ class "cell details" ] [ text <| translate language Translate.Details ]
                ]

        historyRows =
            Dict.toList data
                |> List.reverse
                |> List.tail
                |> Maybe.withDefault []
                |> List.map viewHistoryRow

        viewHistoryRow ( ( month, year ), dataForMonth ) =
            let
                monthForView =
                    Date.numberToMonth month

                yearForView =
                    modBy 1000 year

                startingStock =
                    Maybe.map String.fromFloat dataForMonth.startingStock
                        |> Maybe.withDefault ""

                currentBalance =
                    Maybe.map String.fromFloat dataForMonth.currentBalance
                        |> Maybe.withDefault ""

                monthGap =
                    dateToMonthYear currentDate
                        |> monthYearDiff ( month, year )
            in
            div [ class "row" ]
                [ div [ class "cell month" ] [ text <| translate language <| Translate.ResolveMonthYY monthForView yearForView False ]
                , div [ class "cell starting-stock" ] [ text startingStock ]
                , div [ class "cell received" ] [ text <| String.fromFloat dataForMonth.received ]
                , div [ class "cell issued" ] [ text <| String.fromFloat dataForMonth.issued ]
                , div [ class "cell balance" ] [ text currentBalance ]
                , div [ class "cell details" ]
                    [ button [ onClick <| SetDisplayMode (ModeMonthDetails monthGap) ]
                        [ text <| translate language Translate.View ]
                    ]
                ]
    in
    [ viewMonthSelector language dateLastDayOfSelectedMonth model.monthGap maxMonthGap ChangeMonthGap
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language Translate.MTDIn selectedMonthReceived
            , chwCard language Translate.MTDOut selectedMonthIssued
            , chwCard language Translate.CurrentStock selectedMonthStock
            ]
        ]
    , div
        [ class "navigation-buttons" ]
        [ viewButton (Translate.StockManagementMenu MenuReceiveStock) (SetDisplayMode ModeReceiveStock)
        , viewButton (Translate.StockManagementMenu MenuViewMonthDetails) (SetDisplayMode (ModeMonthDetails 0))
        , viewButton (Translate.StockManagementMenu MenuCorrectEntry) (SetDisplayMode ModeCorrectEntry)
        ]
    , div [ class "pane history" ]
        [ div [ class "pane-heading" ]
            [ text <| translate language Translate.History ]
        , div [ class "pane-content" ] <|
            historyHeaderRow
                :: historyRows
        ]
    , lastUpdated
    ]


viewModeMonthDetails :
    Language
    -> NominalDate
    -> Int
    -> StockManagementData
    -> List (Html Msg)
viewModeMonthDetails language currentDate monthGap data =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate monthGap

        monthYear =
            dateToMonthYear dateLastDayOfSelectedMonth

        headerRow =
            div [ class "row header" ]
                [ div [ class "cell date" ] [ text <| translate language Translate.Date ]
                , div [ class "cell from-to" ]
                    [ text <|
                        (translate language Translate.ReceivedFrom ++ " / " ++ translate language Translate.IssuedTo)
                    ]
                , div [ class "cell batch" ] [ text <| translate language Translate.BatchNumberAbbrev ]
                , div [ class "cell expirity" ] [ text <| translate language Translate.ExpiryDate ]
                , div [ class "cell received" ] [ text <| translate language Translate.Received ]
                , div [ class "cell issued" ] [ text <| translate language Translate.Issued ]
                , div [ class "cell balance" ] [ text <| translate language Translate.Balance ]
                , div [ class "cell months-of-stock" ] [ text <| translate language Translate.MonthsOfStock ]
                , div [ class "cell signature" ] [ text <| translate language Translate.Signature ]
                ]

        rows =
            Dict.get monthYear data
                |> Maybe.map
                    (\dataForMonth ->
                        let
                            stockUpdates =
                                List.map
                                    (\stockUpdate ->
                                        let
                                            fromTo =
                                                Maybe.map (Translate.StockSupplierAbbrev >> translate language) stockUpdate.supplier
                                                    |> Maybe.withDefault ""

                                            ( received, issued ) =
                                                if stockUpdate.quantity < 0 then
                                                    ( Nothing, Just <| toFloat (-1 * stockUpdate.quantity) )

                                                else
                                                    ( Just <| toFloat stockUpdate.quantity, Nothing )
                                        in
                                        { date = stockUpdate.dateRecorded
                                        , fromTo = fromTo
                                        , batch = Maybe.withDefault "" stockUpdate.batchNumber
                                        , expirity = stockUpdate.dateExpires
                                        , received = received
                                        , issued = issued
                                        , signature = Just stockUpdate.signature
                                        , balance = Nothing
                                        }
                                    )
                                    dataForMonth.stockUpdates

                            fbfs =
                                List.map
                                    (\fbf ->
                                        { date = fbf.dateMeasured
                                        , fromTo = translate language Translate.Patients
                                        , batch = ""
                                        , expirity = Nothing
                                        , received = Nothing
                                        , issued = Just fbf.value.distributedAmount

                                        -- Fbf distribution does not have signature.
                                        , signature = Nothing
                                        , balance = Nothing
                                        }
                                    )
                                    dataForMonth.fbfs
                                    |> List.foldl
                                        (\new accum ->
                                            Dict.get new.date accum
                                                |> Maybe.map
                                                    (\stored ->
                                                        let
                                                            combined =
                                                                { date = new.date
                                                                , fromTo = new.fromTo
                                                                , batch = ""
                                                                , expirity = Nothing
                                                                , received = Nothing
                                                                , issued =
                                                                    Maybe.map2 (+)
                                                                        stored.issued
                                                                        new.issued
                                                                , signature = Nothing
                                                                , balance = Nothing
                                                                }
                                                        in
                                                        Dict.insert new.date combined accum
                                                    )
                                                |> Maybe.withDefault (Dict.insert new.date new accum)
                                        )
                                        Dict.empty
                                    |> Dict.values

                            all =
                                stockUpdates
                                    ++ fbfs
                                    |> List.sortWith (sortByDateDesc .date)

                            allWithBalance =
                                Maybe.map
                                    (\startingStock ->
                                        List.reverse all
                                            |> List.indexedMap Tuple.pair
                                            |> List.foldl
                                                (\( index, entry ) accum ->
                                                    let
                                                        balance =
                                                            if index == 0 then
                                                                startingStock
                                                                    + Maybe.withDefault 0 entry.received
                                                                    - Maybe.withDefault 0 entry.issued
                                                                    |> Just

                                                            else
                                                                List.Extra.getAt (index - 1) accum
                                                                    |> Maybe.map
                                                                        (\prev ->
                                                                            Maybe.withDefault 0 prev.balance
                                                                                + Maybe.withDefault 0 entry.received
                                                                                - Maybe.withDefault 0 entry.issued
                                                                        )
                                                    in
                                                    accum ++ [ { entry | balance = balance } ]
                                                )
                                                []
                                            |> List.reverse
                                    )
                                    dataForMonth.startingStock
                                    |> Maybe.withDefault all
                        in
                        List.map (viewRow dataForMonth.consumptionAverage) allWithBalance
                    )
                |> Maybe.withDefault []

        viewRow averageConsumption rowData =
            let
                expirity =
                    Maybe.map formatDDMMYY rowData.expirity
                        |> Maybe.withDefault ""

                received =
                    Maybe.map String.fromFloat rowData.received
                        |> Maybe.withDefault "---"

                issued =
                    Maybe.map String.fromFloat rowData.issued
                        |> Maybe.withDefault "---"

                balance =
                    Maybe.map String.fromFloat rowData.balance
                        |> Maybe.withDefault ""

                monthsOfStock =
                    if averageConsumption == 0 then
                        ""

                    else
                        Maybe.Extra.or rowData.received rowData.issued
                            |> Maybe.map (\quantity -> Round.round 1 (quantity / averageConsumption))
                            |> Maybe.withDefault ""

                signature =
                    Maybe.map viewPhotoThumbFromImageUrl rowData.signature
                        |> Maybe.withDefault emptyNode
            in
            div [ class "row" ]
                [ div [ class "cell date" ] [ text <| formatDDMMYY rowData.date ]
                , div [ class "cell from-to" ] [ text rowData.fromTo ]
                , div [ class "cell batch" ] [ text rowData.batch ]
                , div [ class "cell expirity" ] [ text expirity ]
                , div [ class "cell received" ] [ text received ]
                , div [ class "cell issued" ] [ text issued ]
                , div [ class "cell balance" ] [ text balance ]
                , div [ class "cell months-of-stock" ] [ text monthsOfStock ]
                , div [ class "cell signature" ] [ signature ]
                ]
    in
    [ viewMonthSelector language dateLastDayOfSelectedMonth monthGap maxMonthGap ChangeDetailsMonthGap
    , div [ class "pane month-details" ]
        [ div [ class "pane-heading" ]
            [ text <| translate language Translate.History ]
        , div [ class "pane-content" ] <|
            headerRow
                :: rows
        ]
    ]


viewModeReceiveStock : Language -> NominalDate -> NurseId -> Nurse -> Maybe Float -> ReceiveStockForm -> List (Html Msg)
viewModeReceiveStock language currentDate nurseId nurse consumptionAverage form =
    let
        ( inputs, tasks ) =
            let
                ( derivedInputs, derivedTasks ) =
                    if form.confirmIdentity == Just True then
                        let
                            dateRecordedSelectorConfig =
                                let
                                    fromDate =
                                        Date.add Years -5 currentDate
                                in
                                { select = SetDateRecorded
                                , close = SetDateRecordedSelectorState Nothing
                                , dateFrom = fromDate
                                , dateTo = currentDate
                                , dateDefault = Just currentDate
                                }

                            dateRecordedForView =
                                Maybe.map formatDDMMYYYY form.dateRecorded
                                    |> Maybe.withDefault ""

                            dateExpiresSelectorConfig =
                                let
                                    toDate =
                                        Date.add Years 10 currentDate
                                in
                                { select = SetDateExpires
                                , close = SetDateExpiresSelectorState Nothing
                                , dateFrom = currentDate
                                , dateTo = toDate
                                , dateDefault = Just toDate
                                }

                            dateExpiresForView =
                                Maybe.map formatDDMMYYYY form.dateExpires
                                    |> Maybe.withDefault ""

                            monthsOfStock =
                                Maybe.map2
                                    (\quantity averageConsumption ->
                                        if averageConsumption == 0 then
                                            []

                                        else
                                            [ viewLabel language Translate.MonthsOfStock
                                            , div [ class "label months-of-stock" ] [ text <| Round.round 1 (toFloat quantity / averageConsumption) ]
                                            ]
                                    )
                                    form.quantity
                                    consumptionAverage
                                    |> Maybe.withDefault []
                        in
                        ( [ viewLabel language Translate.StockManagementSelectDateLabel
                          , div
                                [ class "form-input date"
                                , onClick <| SetDateRecordedSelectorState (Just dateRecordedSelectorConfig)
                                ]
                                [ text dateRecordedForView ]
                          , viewModal <| viewCalendarPopup language form.dateRecordedSelectorPopupState form.dateRecorded
                          , viewQuestionLabel language Translate.StockManagementSupplierQuestion
                          , viewSelectListInput language
                                form.supplier
                                [ SupplierMOH
                                , SupplierRBC
                                , SupplierUNICEF
                                , SupplierRMSCentral
                                , SupplierRMSDistrict
                                , SupplierBUFMAR
                                ]
                                stockSupplierToString
                                SetStockSupplier
                                Translate.StockSupplier
                                "select"
                          , viewQuestionLabel language Translate.StockManagementBatchNumberQuestion
                          , viewTextInput language
                                (Maybe.withDefault "" form.batchNumber)
                                SetBatchNumber
                                Nothing
                                (Just "form-input batch-number")
                          , viewQuestionLabel language Translate.StockManagementDateExpiresQuestion
                          , div
                                [ class "form-input date"
                                , onClick <| SetDateExpiresSelectorState (Just dateExpiresSelectorConfig)
                                ]
                                [ text dateExpiresForView ]
                          , viewModal <| viewCalendarPopup language form.dateExpiresSelectorPopupState form.dateExpires
                          , viewQuestionLabel language Translate.StockManagementQuantityAddedQuestion
                          , viewNumberInput
                                form.quantity
                                SetQuantityAdded
                                "quantity"
                          ]
                            ++ monthsOfStock
                            ++ [ viewLabel language Translate.Observations
                               , textarea
                                    [ rows 5
                                    , cols 50
                                    , class "form-input textarea"
                                    , value <| Maybe.withDefault "" form.notes
                                    , onInput SetNotes
                                    ]
                                    []
                               ]
                            ++ viewSignature language ReceiveStockHandleStoredSignature form.signature
                        , [ maybeToBoolTask form.dateRecorded
                          , maybeToBoolTask form.supplier
                          , maybeToBoolTask form.batchNumber
                          , maybeToBoolTask form.dateExpires
                          , maybeToBoolTask form.quantity
                          , maybeToBoolTask form.signature
                          ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewLoggedInAsPhrase language nurse
              , viewBoolInput language
                    form.confirmIdentity
                    SetReceiveStockConfirmIdentity
                    "confirm-identity"
                    Nothing
              ]
                ++ derivedInputs
            , form.confirmIdentity :: derivedTasks
            )

        ( tasksCompleted, totalTasks ) =
            resolveTasksCompletedFromTotal tasks
    in
    viewStockUpdateContent language
        form.confirmIdentity
        inputs
        (SaveReceiveStock nurseId)
        form.displayIdentityPopup
        HideReceiveStockIdentityPopup
        tasksCompleted
        totalTasks


viewModeCorrectEntry : Language -> NominalDate -> NurseId -> Nurse -> CorrectEntryForm -> List (Html Msg)
viewModeCorrectEntry language currentDate nurseId nurse form =
    let
        ( inputs, tasks ) =
            let
                ( derivedInputs, derivedTasks ) =
                    if form.confirmIdentity == Just True then
                        let
                            dateSelectorConfig =
                                let
                                    fromDate =
                                        Date.add Years -5 currentDate
                                in
                                { select = SetDate
                                , close = SetDateSelectorState Nothing
                                , dateFrom = fromDate
                                , dateTo = currentDate
                                , dateDefault = Just currentDate
                                }

                            dateForView =
                                Maybe.map formatDDMMYYYY form.date
                                    |> Maybe.withDefault ""

                            ( correctionReasonInputs, correctionReasonTasks ) =
                                let
                                    -- Change of value at input that affects display of conditional input,
                                    -- and located above signature pad causes pad to unbind, and signature
                                    -- to disappear.
                                    -- To overcome this, conditional input is changed to constant,
                                    -- and it is hidden with CSS, until needed field on form is set.
                                    correctionReasonSection entryType hidden =
                                        let
                                            reasons =
                                                case entryType of
                                                    EntryAddition ->
                                                        [ ReasonInputError
                                                        , ReasonOther
                                                        ]

                                                    EntrySubstraction ->
                                                        [ ReasonInputError
                                                        , ReasonExpiration
                                                        , ReasonMissing
                                                        , ReasonOther
                                                        ]
                                        in
                                        div
                                            [ classList
                                                [ ( "correction-reason", True )
                                                , ( "hidden", hidden )
                                                ]
                                            ]
                                            [ viewQuestionLabel language Translate.StockManagementCorrectionReasonLabel
                                            , viewCheckBoxSelectInput language
                                                reasons
                                                []
                                                form.reason
                                                SetCorrectionReason
                                                Translate.StockCorrectionReason
                                            ]
                                in
                                Maybe.map
                                    (\entryType ->
                                        ( [ correctionReasonSection entryType False ]
                                        , [ maybeToBoolTask form.reason ]
                                        )
                                    )
                                    form.entryType
                                    |> Maybe.withDefault
                                        ( [ -- Since entryType is not set yet, we display the input
                                            -- at 'hidden' mode.
                                            correctionReasonSection EntrySubstraction True
                                          ]
                                        , []
                                        )
                        in
                        ( [ viewLabel language Translate.StockManagementSelectDateLabel
                          , div
                                [ class "form-input date"
                                , onClick <| SetDateSelectorState (Just dateSelectorConfig)
                                ]
                                [ text dateForView ]
                          , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.date
                          , viewLabel language Translate.StockManagementCorrectionTypeLabel
                          , viewSelectListInput language
                                form.entryType
                                [ EntryAddition, EntrySubstraction ]
                                correctionEntryTypeToString
                                SetCorrectionEntryType
                                Translate.StockManagementCorrectionEntryType
                                "correction-type"
                          ]
                            ++ correctionReasonInputs
                            ++ [ viewLabel language Translate.StockManagementQuantityCorrectionLabel
                               , viewNumberInput
                                    form.quantity
                                    SetQuantityDeducted
                                    "quantity"
                               ]
                            ++ viewSignature language CorrectEntryHandleStoredSignature form.signature
                        , [ maybeToBoolTask form.date
                          , maybeToBoolTask form.entryType
                          , maybeToBoolTask form.quantity
                          , maybeToBoolTask form.signature
                          ]
                            ++ correctionReasonTasks
                        )

                    else
                        ( [], [] )
            in
            ( [ viewLoggedInAsPhrase language nurse
              , viewBoolInput language
                    form.confirmIdentity
                    SetCorrectEntryConfirmIdentity
                    "confirm-identity"
                    Nothing
              ]
                ++ derivedInputs
            , form.confirmIdentity :: derivedTasks
            )

        ( tasksCompleted, totalTasks ) =
            resolveTasksCompletedFromTotal tasks
    in
    viewStockUpdateContent language
        form.confirmIdentity
        inputs
        (SaveCorrectEntry nurseId)
        form.displayIdentityPopup
        HideCorrectEntryIdentityPopup
        tasksCompleted
        totalTasks


viewLoggedInAsPhrase : Language -> Nurse -> Html any
viewLoggedInAsPhrase language nurse =
    p [ class "label logged-in" ]
        [ text <| translate language Translate.LoggedInAsPhrase
        , span [] [ text nurse.name ]
        , text ". "
        , text <| translate language Translate.IsThisYouQuestion
        , text "?"
        ]


viewSignature : Language -> (String -> Msg) -> Maybe ImageUrl -> List (Html Msg)
viewSignature language handleStoredSignatureMsg signature =
    let
        content =
            Maybe.map
                (\(ImageUrl url) ->
                    div [ class "signature" ]
                        [ img [ src url ] [] ]
                )
                signature
                |> Maybe.withDefault (viewSignaturePad language handleStoredSignatureMsg)
    in
    [ viewLabel language Translate.StockManagementEnterSignatureLabel
    , content
    ]


viewSignaturePad : Language -> (String -> Msg) -> Html Msg
viewSignaturePad language handleStoredSignatureMsg =
    div
        [ class "signature-pad"
        , id "signature-pad"
        , on "signaturecomplete" (Json.Decode.map handleStoredSignatureMsg (Json.Decode.at [ "detail", "url" ] Json.Decode.string))
        ]
        [ div
            [ class "signature-pad--body" ]
            [ canvas [] [] ]
        , div
            [ class "signature-pad--footer" ]
            [ button
                [ class "primary"
                , onClick StoreSignature
                ]
                [ text <| translate language Translate.Accept ]
            , button [ onClick ClearSignaturePad ] [ text <| translate language Translate.Clear ]
            ]
        ]


viewStockUpdateContent : Language -> Maybe Bool -> List (Html Msg) -> Msg -> Bool -> Msg -> Int -> Int -> List (Html Msg)
viewStockUpdateContent language confirmIdentity formForView saveMsg displayPopup hidePopupMsg tasksCompleted totalTasks =
    [ div [ class "ui unstackable items" ]
        [ viewTasksCount language tasksCompleted totalTasks
        , div [ class "ui full segment" ]
            [ div [ class "ui full content" ]
                formForView
            , viewSaveAction language
                saveMsg
                (tasksCompleted /= totalTasks)
                |> showIf (confirmIdentity == Just True)
            ]
        , viewModal <|
            identityPopup language displayPopup hidePopupMsg
        ]
    ]


identityPopup : Language -> Bool -> Msg -> Maybe (Html Msg)
identityPopup language displayed hideMsg =
    if displayed then
        Just <|
            customPopup language
                False
                Translate.OK
                "warning-popup"
                ( p [] [ text <| translate language Translate.IdentityPopupHeader ]
                , p [] [ text <| translate language Translate.IdentityPopupInstructions ]
                , hideMsg
                )

    else
        Nothing
