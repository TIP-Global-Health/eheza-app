module Pages.StockManagement.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( Gender(..)
        , StockCorrectionReason(..)
        , StockSupplier(..)
        , StockUpdate
        , StockUpdateType(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Utils exposing (sortByDate, sortByDateDesc)
import Backend.StockUpdate.Model exposing (StockManagementData)
import Backend.StockUpdate.Utils exposing (..)
import Date exposing (Month, Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYY, formatDDMMYYYY, fromLocalDateTime)
import Gizra.TimePosix exposing (viewTimePosix)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import List.Zipper exposing (Zipper)
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Dashboard.View exposing (chwCard)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.StockManagement.Model exposing (..)
import Pages.StockManagement.Utils exposing (..)
import Pages.Utils
    exposing
        ( customPopup
        , maybeToBoolTask
        , resolveSelectedDateForMonthSelector
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMonthSelector
        , viewNumberInput
        , viewQuestionLabel
        , viewSaveAction
        , viewSelectListInput
        , viewTextInput
        )
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Round
import SyncManager.Model exposing (SyncInfoAuthorityZipper)
import Time
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (spinner, viewModal)
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
            let
                lastUpdated =
                    viewLastUpdated language maybeHealthCenterId syncInfoAuthorities
            in
            case model.displayMode of
                ModeMain ->
                    viewModeMain language currentDate nurseId nurse lastUpdated data model

                ModeMonthDetails monthGap ->
                    viewModeMonthDetails language currentDate monthGap lastUpdated data

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
    -> NurseId
    -> Nurse
    -> Html Msg
    -> StockManagementData
    -> Model
    -> List (Html Msg)
viewModeMain language currentDate nurseId nurse lastUpdated data model =
    let
        viewButton label action =
            button
                [ class "ui primary button"
                , onClick action
                ]
                [ span [ class "text" ] [ text <| translate language label ]
                , span [ class "icon-back" ] []
                ]

        selectedDate =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        ( selectedMonthReceived, selectedMonthIssued, selectedMonthStock ) =
            let
                selectedMonthYear =
                    dateToMonthYear selectedDate
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
    [ viewMonthSelector language selectedDate model.monthGap maxMonthGap ChangeMonthGap
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
    -> Html Msg
    -> StockManagementData
    -> List (Html Msg)
viewModeMonthDetails language currentDate monthGap lastUpdated data =
    let
        selectedDate =
            resolveSelectedDateForMonthSelector currentDate monthGap

        monthYear =
            dateToMonthYear selectedDate

        headerRow =
            div [ class "row header" ]
                [ div [ class "cell date" ] [ text <| translate language Translate.Date ]
                , div [ class "cell from-to" ]
                    [ text <|
                        (translate language Translate.ReceivedFrom ++ " / " ++ translate language Translate.IssuedTo)
                    ]
                , div [ class "cell batch" ] [ text <| translate language Translate.BatchNumberAbbrev ]
                , div [ class "cell expirity" ] [ text <| translate language Translate.ExpirityDate ]
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
                , div [ class "cell signature" ] [ text "@todo" ]
                ]
    in
    [ viewMonthSelector language selectedDate monthGap maxMonthGap ChangeDetailsMonthGap
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
                          , viewNumberInput language
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
                            ++ viewSignaturePad language
                        , [ maybeToBoolTask form.dateRecorded
                          , maybeToBoolTask form.supplier
                          , maybeToBoolTask form.batchNumber
                          , maybeToBoolTask form.dateExpires
                          , maybeToBoolTask form.quantity
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
            , [ form.confirmIdentity ] ++ derivedTasks
            )

        ( tasksCompleted, totalTasks ) =
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )
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
                                Maybe.map
                                    (\entryType ->
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
                                        ( [ viewQuestionLabel language Translate.StockManagementCorrectionReasonLabel
                                          , viewCheckBoxSelectInput language
                                                reasons
                                                []
                                                form.reason
                                                SetCorrectionReason
                                                Translate.StockCorrectionReason
                                          ]
                                        , [ maybeToBoolTask form.reason ]
                                        )
                                    )
                                    form.entryType
                                    |> Maybe.withDefault ( [], [] )
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
                          , viewLabel language Translate.StockManagementQuantityCorrectionLabel
                          , viewNumberInput language
                                form.quantity
                                SetQuantityDeducted
                                "quantity"
                          ]
                            ++ correctionReasonInputs
                            ++ viewSignaturePad language
                        , [ maybeToBoolTask form.date
                          , maybeToBoolTask form.entryType
                          , maybeToBoolTask form.quantity
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
            , [ form.confirmIdentity ] ++ derivedTasks
            )

        ( tasksCompleted, totalTasks ) =
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )
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


viewSignaturePad : Language -> List (Html Msg)
viewSignaturePad language =
    [ viewLabel language Translate.StockManagementEnterSignatureLabel
    , div
        [ class "signature-pad"
        , id "signature-pad"

        -- , on "signatureupdate" (Json.Decode.map SignatureUpdate decodeSignature)
        ]
        [ div
            [ class "signature-pad--body" ]
            [ canvas [] [] ]
        , div
            [ class "signature-pad--footer" ]
            [ button [ onClick ClearSignaturePad ] [ text <| translate language Translate.Clear ] ]
        ]
    ]


viewStockUpdateContent : Language -> Maybe Bool -> List (Html Msg) -> Msg -> Bool -> Msg -> Int -> Int -> List (Html Msg)
viewStockUpdateContent language confirmIdentity formForView saveMsg displayPopup hidePopupMsg tasksCompleted totalTasks =
    [ div [ class "ui unstackable items" ]
        [ div [ class "tasks-count" ]
            [ text <|
                translate language <|
                    Translate.TasksCompleted tasksCompleted totalTasks
            ]
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
                ( p [] [ text <| translate language Translate.IdentityPopupHeader ]
                , p [] [ text <| translate language Translate.IdentityPopupInstructions ]
                , hideMsg
                )

    else
        Nothing
