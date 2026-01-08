module Pages.HealthyStart.Encounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.HealthyStartEncounter.Model
import Backend.Model
import Pages.HealthyStart.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))


update : HealthyStartEncounterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id msg model =
    case msg of
        CloseEncounter ->
            ( model
            , Cmd.none
            , [ Backend.HealthyStartEncounter.Model.CloseEncounter
                    |> Backend.Model.MsgHealthyStartEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            let
                appMsgs =
                    -- @todo
                    -- case page of
                    --     UserPage (HealthyStartActivityPage _ NextSteps) ->
                    --         Pages.HealthyStart.Activity.Model.SetWarningPopupState (Just WarningPopupRegular)
                    --             |> App.Model.MsgPageHealthyStartActivity id Backend.HealthyStartActivity.Model.NextSteps
                    --             |> App.Model.MsgLoggedIn
                    --             |> List.singleton
                    --
                    --     _ ->
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
