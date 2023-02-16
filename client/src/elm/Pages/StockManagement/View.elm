module Pages.StockManagement.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Utils exposing (sortByDateDesc, sortEncounterTuplesDesc)
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
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSelectListInput
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

                        loggedInAsPhrase =
                            p [ class "label logged-in" ]
                                [ text <| translate language Translate.LoggedInAsPhrase
                                , span [] [ text nurse.name ]
                                , text ". "
                                , text <| translate language Translate.IsThisYouQuestion
                                , text "?"
                                ]
                    in
                    [ div [ class "ui unstackable items" ]
                        [ div [ class "ui full segment" ]
                            [ loggedInAsPhrase
                            , viewBoolInput language
                                form.confirmIdentity
                                SetConfirmIdentity
                                "confirm-identity"
                                Nothing
                            , viewModal <|
                                identityPopup language form.displayIdentityPopup
                            ]
                        ]
                    ]

                ModeCorrectEntry ->
                    []
    in
    div [ class "page-activity stock-management" ] <|
        header
            :: content


identityPopup : Language -> Bool -> Maybe (Html Msg)
identityPopup language displayed =
    if displayed then
        Just <|
            customPopup language
                False
                Translate.OK
                ( p [] [ text <| translate language Translate.IdentityPopupHeader ]
                , p [] [ text <| translate language Translate.IdentityPopupInstructions ]
                , HideIdentityPopup
                )

    else
        Nothing
