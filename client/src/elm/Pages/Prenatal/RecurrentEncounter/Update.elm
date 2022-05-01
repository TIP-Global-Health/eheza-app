module Pages.Prenatal.RecurrentEncounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.Model
import Backend.PrenatalActivity.Model exposing (PrenatalRecurrentActivity(..))
import Backend.PrenatalEncounter.Model
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.RecurrentActivity.Model
import Pages.Prenatal.RecurrentEncounter.Model exposing (..)


update : PrenatalEncounterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id msg model =
    case msg of
        CloseEncounter ->
            ( model
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.ClosePrenatalEncounter
                    |> Backend.Model.MsgPrenatalEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            let
                appMsgs =
                    case page of
                        UserPage (PrenatalRecurrentActivityPage _ RecurrentNextSteps) ->
                            Pages.Prenatal.RecurrentActivity.Model.ViewWarningPopupForNonUrgentDiagnoses
                                |> App.Model.MsgPagePrenatalRecurrentActivity id Backend.PrenatalActivity.Model.RecurrentNextSteps
                                |> App.Model.MsgLoggedIn
                                |> List.singleton

                        _ ->
                            []
            in
            ( model
            , Cmd.none
            , App.Model.SetActivePage page :: appMsgs
            )

        SetAlertsDialogState value ->
            ( { model | showAlertsDialog = value }, Cmd.none, [] )

        SetEndEncounterDialogState value ->
            ( { model | showEndEncounterDialog = value }, Cmd.none, [] )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )
