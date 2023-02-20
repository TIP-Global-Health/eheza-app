module Pages.StockManagement.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Utils exposing (sortByDateDesc, sortEncounterTuplesDesc)
import Backend.StockUpdate.Model exposing (StockSupplier(..))
import Backend.StockUpdate.Utils exposing (stockSupplierToString)
import Date exposing (Month, Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.StockManagement.Model exposing (..)
import Pages.Utils
    exposing
        ( customPopup
        , maybeToBoolTask
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
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


view : Language -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurseId nurse db model =
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
            let
                loggedInAsPhrase =
                    p [ class "label logged-in" ]
                        [ text <| translate language Translate.LoggedInAsPhrase
                        , span [] [ text nurse.name ]
                        , text ". "
                        , text <| translate language Translate.IsThisYouQuestion
                        , text "?"
                        ]

                viewStockUpdateContent confirmIdentity formForView saveMsg displayPopup hidePopupMsg tasksCompleted totalTasks =
                    [ div [ class "ui unstackable items" ]
                        [ div [ class "tasks-count" ]
                            [ text <|
                                translate language <|
                                    Translate.TasksCompleted tasksCompleted totalTasks
                            ]
                        , div [ class "ui full segment" ]
                            [ div [ class "ui full content" ] <|
                                formForView
                                    ++ [ viewSaveAction language
                                            saveMsg
                                            (confirmIdentity == Just True && tasksCompleted == totalTasks)
                                       ]
                            ]
                        , viewModal <|
                            identityPopup language displayPopup hidePopupMsg
                        ]
                    ]
            in
            case model.displayMode of
                ModeMain ->
                    let
                        viewButton label action =
                            button
                                [ class "ui primary button"
                                , onClick action
                                ]
                                [ span [ class "text" ] [ text <| translate language label ]
                                , span [ class "icon-back" ] []
                                ]
                    in
                    [ div [ class "navigation-buttons" ]
                        [ viewButton (Translate.StockManagementMenu MenuReceiveStock) (SetDisplayMode ModeReceiveStock)
                        , viewButton (Translate.StockManagementMenu MenuViewMonthDetails) (SetDisplayMode ModeCorrectEntry)
                        , viewButton (Translate.StockManagementMenu MenuCorrectEntry) (SetDisplayMode ModeCorrectEntry)
                        ]
                    ]

                ModeReceiveStock ->
                    let
                        form =
                            model.receiveStockForm

                        ( inputs, tasks ) =
                            let
                                ( derivedInputs, derivedTasks ) =
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
                                            , dateDefault = Just fromDate
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
                                    if form.confirmIdentity == Just True then
                                        ( [ viewLabel language Translate.ReceiveStockSelectDateLabel
                                          , div
                                                [ class "form-input date"
                                                , onClick <| SetDateRecordedSelectorState (Just dateRecordedSelectorConfig)
                                                ]
                                                [ text dateRecordedForView ]
                                          , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.dateRecorded
                                          , viewQuestionLabel language Translate.ReceiveStockSupplierQuestion
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
                                          , viewQuestionLabel language Translate.ReceiveStockBatchNumberQuestion
                                          , viewTextInput language
                                                form.batchNumber
                                                SetBatchNumber
                                                Nothing
                                                (Just "batch-number")
                                          , viewQuestionLabel language Translate.ReceiveStockDateExpiresQuestion
                                          , div
                                                [ class "form-input date"
                                                , onClick <| SetDateExpiresSelectorState (Just dateExpiresSelectorConfig)
                                                ]
                                                [ text dateExpiresForView ]
                                          , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.dateExpires
                                          , viewQuestionLabel language Translate.ReceiveStockQuantityAddedQuestion
                                          , viewNumberInput language
                                                form.quantity
                                                SetQuantityAdded
                                                "quantity"
                                          ]
                                        , [ maybeToBoolTask form.dateRecorded
                                          , maybeToBoolTask form.supplier
                                          , if String.isEmpty form.batchNumber then
                                                Nothing

                                            else
                                                Just True
                                          , maybeToBoolTask form.dateExpires
                                          , maybeToBoolTask form.quantity
                                          ]
                                        )

                                    else
                                        ( [], [] )
                            in
                            ( [ loggedInAsPhrase
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
                    viewStockUpdateContent form.confirmIdentity
                        inputs
                        SaveReceiveStock
                        form.displayIdentityPopup
                        HideReceiveStockIdentityPopup
                        tasksCompleted
                        totalTasks

                ModeCorrectEntry ->
                    let
                        form =
                            model.correctEntryForm

                        ( inputs, tasks ) =
                            let
                                ( derivedInputs, derivedTasks ) =
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
                                    in
                                    if form.confirmIdentity == Just True then
                                        ( [ viewLabel language Translate.ReceiveStockSelectDateLabel
                                          , div
                                                [ class "form-input date"
                                                , onClick <| SetDateSelectorState (Just dateSelectorConfig)
                                                ]
                                                [ text dateForView ]
                                          , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.date
                                          , viewQuestionLabel language Translate.ReceiveStockQuantityDeductedQuestion
                                          , viewNumberInput language
                                                form.quantity
                                                SetQuantityDeducted
                                                "quantity"
                                          ]
                                        , [ maybeToBoolTask form.date
                                          , maybeToBoolTask form.quantity
                                          ]
                                        )

                                    else
                                        ( [], [] )
                            in
                            ( [ loggedInAsPhrase
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
                    viewStockUpdateContent form.confirmIdentity
                        inputs
                        SaveCorrectEntry
                        form.displayIdentityPopup
                        HideCorrectEntryIdentityPopup
                        tasksCompleted
                        totalTasks
    in
    div [ class "page-activity stock-management" ] <|
        header
            :: content


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
