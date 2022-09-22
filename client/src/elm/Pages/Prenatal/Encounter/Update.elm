module Pages.Prenatal.Encounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
import Backend.PrenatalEncounter.Model
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Model
import Pages.Prenatal.Activity.Types exposing (WarningPopupType(..))
import Pages.Prenatal.Encounter.Model exposing (..)


update : PrenatalEncounterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id msg model =
    case msg of
        CloseEncounter ->
            ( model
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.CloseEncounter
                    |> Backend.Model.MsgPrenatalEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            let
                appMsgs =
                    case page of
                        UserPage (PrenatalActivityPage _ NextSteps) ->
                            Pages.Prenatal.Activity.Model.SetWarningPopupState (Just WarningPopupRegular)
                                |> App.Model.MsgPagePrenatalActivity id Backend.PrenatalActivity.Model.NextSteps
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

        SetChwWarningVisible value ->
            ( { model | showWarningForChw = value }, Cmd.none, [] )

        SetEndEncounterDialogState value ->
            ( { model | showEndEncounterDialog = value }, Cmd.none, [] )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        UndeterminedDiagnosesWarningAcknowledged ->
            ( { model | undeterminedDiagnosesWarningAcknowledged = True }, Cmd.none, [] )
