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
import Backend.NutritionEncounter.Utils exposing (sortByDate)
import Backend.StockUpdate.Utils exposing (stockSupplierToString)
import Date exposing (Month, Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Maybe HealthCenterId -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentDate maybeHealthCenterId nurseId nurse db model =
    let
        header =
            let
                ( label, action ) =
                    case model.displayMode of
                        ModeMain ->
                            ( Translate.StockManagement, SetActivePage PinCodePage )

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
                    viewModeMain language currentDate maybeHealthCenterId nurseId nurse db model

                ModeReceiveStock ->
                    viewModeReceiveStock language currentDate nurseId nurse model.receiveStockForm

                ModeCorrectEntry ->
                    viewModeCorrectEntry language currentDate nurseId nurse model.correctEntryForm
    in
    div [ class "page-activity stock-management" ] <|
        header
            :: content


viewModeMain : Language -> NominalDate -> Maybe HealthCenterId -> NurseId -> Nurse -> ModelIndexedDb -> Model -> List (Html Msg)
viewModeMain language currentDate maybeHealthCenterId nurseId nurse db model =
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

        historyData =
            Maybe.andThen
                (\healthCenterId ->
                    Dict.get healthCenterId db.stockManagementMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (\measurements ->
                                let
                                    firstMonthForDisplay =
                                        Date.add Date.Months -12 currentDate
                                            |> dateToMonthYear

                                    firstStockUpdateMonthYear =
                                        Dict.values measurements.stockUpdate
                                            |> List.sortWith (sortByDate .dateRecorded)
                                            |> List.head
                                            |> Maybe.map .dateRecorded
                                            |> Maybe.withDefault
                                                -- When no stock updates were found, we know that
                                                -- health center is yet to activate Stock Management
                                                -- feature, because when activated, initial quantity
                                                -- should be set.
                                                -- Inticatin of this would be setting first stock upddate
                                                -- to future date.
                                                (Date.add Date.Months 1 currentDate)
                                            |> dateToMonthYear

                                    fbfForDisplay =
                                        Dict.values measurements.childFbf
                                            ++ Dict.values measurements.motherFbf
                                            |> List.filter
                                                (\fbf ->
                                                    let
                                                        fbfMonthYear =
                                                            dateToMonthYear fbf.dateMeasured
                                                    in
                                                    compareMonthYear fbfMonthYear firstMonthForDisplay /= LT
                                                )

                                    stockUpdateForDisplay =
                                        Dict.values measurements.stockUpdate
                                            |> List.filter
                                                (\stockUpdate ->
                                                    let
                                                        stockUpdateMonthYear =
                                                            dateToMonthYear stockUpdate.dateRecorded
                                                    in
                                                    compareMonthYear stockUpdateMonthYear firstMonthForDisplay /= LT
                                                )

                                    fbfBalance =
                                        Dict.values measurements.childFbf
                                            ++ Dict.values measurements.motherFbf
                                            |> List.filterMap
                                                (\fbf ->
                                                    let
                                                        fbfMonthYear =
                                                            dateToMonthYear fbf.dateMeasured
                                                    in
                                                    if compareMonthYear fbfMonthYear firstStockUpdateMonthYear == EQ then
                                                        Just fbf.value.distributedAmount

                                                    else
                                                        Nothing
                                                )
                                            |> List.sum

                                    stockUpdateBalance =
                                        Dict.values measurements.stockUpdate
                                            |> List.filterMap
                                                (\stockUpdate ->
                                                    let
                                                        stockUpdateMonthYear =
                                                            dateToMonthYear stockUpdate.dateRecorded
                                                    in
                                                    if compareMonthYear stockUpdateMonthYear firstStockUpdateMonthYear == EQ then
                                                        Just stockUpdate.quantity

                                                    else
                                                        Nothing
                                                )
                                            |> List.sum

                                    initialBalance =
                                        toFloat stockUpdateBalance - fbfBalance

                                    receivedIssuedByMonthYear =
                                        List.range 0 12
                                            |> List.map
                                                (\monthGap ->
                                                    let
                                                        monthYear =
                                                            Date.add Date.Months (-1 * monthGap) currentDate
                                                                |> dateToMonthYear

                                                        received =
                                                            List.filter
                                                                (\stockUpdate ->
                                                                    let
                                                                        stockUpdateMonthYear =
                                                                            dateToMonthYear stockUpdate.dateRecorded
                                                                    in
                                                                    compareMonthYear stockUpdateMonthYear monthYear == EQ
                                                                )
                                                                stockUpdateForDisplay
                                                                |> List.map .quantity
                                                                |> List.sum
                                                                |> toFloat

                                                        issued =
                                                            List.filter
                                                                (\fbf ->
                                                                    let
                                                                        fbfMonthYear =
                                                                            dateToMonthYear fbf.dateMeasured
                                                                    in
                                                                    compareMonthYear fbfMonthYear monthYear == EQ
                                                                )
                                                                fbfForDisplay
                                                                |> List.map (.value >> .distributedAmount)
                                                                |> List.sum
                                                    in
                                                    ( monthYear, ( received, issued ) )
                                                )
                                in
                                List.foldr
                                    (\( monthYear, ( received, issued ) ) accum ->
                                        let
                                            prevMonthYear =
                                                getPrevMonthYear monthYear

                                            monthYearBalance =
                                                let
                                                    checkIfInitialBalance =
                                                        if compareMonthYear prevMonthYear firstStockUpdateMonthYear == EQ then
                                                            Just initialBalance

                                                        else
                                                            Nothing
                                                in
                                                Dict.get prevMonthYear accum
                                                    |> Maybe.map
                                                        (\( maybePrevBalance, prevReceived, prevIssued ) ->
                                                            case maybePrevBalance of
                                                                Just prevBalance ->
                                                                    Just <| prevBalance + prevReceived - prevIssued

                                                                Nothing ->
                                                                    checkIfInitialBalance
                                                        )
                                                    |> Maybe.withDefault checkIfInitialBalance
                                        in
                                        Dict.insert monthYear ( monthYearBalance, received, issued ) accum
                                    )
                                    Dict.empty
                                    receivedIssuedByMonthYear
                            )
                )
                maybeHealthCenterId
                |> Maybe.withDefault Dict.empty

        ( selectedMonthReceived, selectedMonthIssued, selectedMonthStock ) =
            let
                selectedMonthYear =
                    dateToMonthYear selectedDate
            in
            Dict.get selectedMonthYear historyData
                |> Maybe.map
                    (\( balance, received, issued ) ->
                        ( String.fromFloat received
                        , String.fromFloat issued
                        , Maybe.map
                            (\initial ->
                                String.fromFloat (initial + received - issued)
                            )
                            balance
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
            Dict.toList historyData
                |> List.reverse
                |> List.tail
                |> Maybe.withDefault []
                |> List.map viewHistoryRow

        viewHistoryRow ( ( month, year ), ( balance, received, issued ) ) =
            let
                monthForView =
                    Date.numberToMonth month

                yearForView =
                    modBy 1000 year

                initialBalance =
                    Maybe.map String.fromFloat balance
                        |> Maybe.withDefault ""

                currentBalance =
                    Maybe.map
                        (\initial ->
                            (initial + received - issued)
                                |> String.fromFloat
                        )
                        balance
                        |> Maybe.withDefault ""
            in
            div [ class "row" ]
                [ div [ class "cell month" ] [ text <| translate language <| Translate.ResolveMonthYY monthForView yearForView False ]
                , div [ class "cell starting-stock" ] [ text initialBalance ]
                , div [ class "cell received" ] [ text <| String.fromFloat received ]
                , div [ class "cell issued" ] [ text <| String.fromFloat issued ]
                , div [ class "cell balance" ] [ text currentBalance ]
                , div [ class "cell details" ] [ button [] [ text <| translate language Translate.View ] ]
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
        , viewButton (Translate.StockManagementMenu MenuViewMonthDetails) (SetDisplayMode ModeCorrectEntry)
        , viewButton (Translate.StockManagementMenu MenuCorrectEntry) (SetDisplayMode ModeCorrectEntry)
        ]
    , div [ class "pane history" ]
        [ div [ class "pane-heading" ]
            [ text <| translate language Translate.History ]
        , div [ class "pane-content" ] <|
            historyHeaderRow
                :: historyRows
        ]
    ]


viewModeReceiveStock : Language -> NominalDate -> NurseId -> Nurse -> ReceiveStockForm -> List (Html Msg)
viewModeReceiveStock language currentDate nurseId nurse form =
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
                          , viewLabel language Translate.Observations
                          , textarea
                                [ rows 5
                                , cols 50
                                , class "form-input textarea"
                                , value <| Maybe.withDefault "" form.notes
                                , onInput SetNotes
                                ]
                                []
                          ]
                        , [ maybeToBoolTask form.dateRecorded
                          , maybeToBoolTask form.supplier
                          , maybeToBoolTask form.batchNumber
                          , maybeToBoolTask form.dateExpires
                          , maybeToBoolTask form.quantity
                          , maybeToBoolTask form.notes
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
                                , dateDefault = Just fromDate
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
                                "correction-reason"
                          , viewLabel language Translate.StockManagementQuantityCorrectionLabel
                          , viewNumberInput language
                                form.quantity
                                SetQuantityDeducted
                                "quantity"
                          ]
                            ++ correctionReasonInputs
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
