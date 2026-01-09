module Pages.HealthyStart.RecurrentEncounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (HealthyStartEncounterId)
import Backend.HealthyStartEncounter.Model
import Backend.Model
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.HealthyStart.RecurrentEncounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))


update : NominalDate -> HealthyStartEncounterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        SetActivePage page ->
            let
                appMsgs =
                    -- @todo
                    -- case page of
                    --     UserPage (HealthyStartRecurrentActivityPage _ RecurrentNextSteps) ->
                    --         Pages.HealthyStart.RecurrentActivity.Model.SetWarningPopupState (Just WarningPopupRegular)
                    --             |> App.Model.MsgPageHealthyStartRecurrentActivity id Backend.HealthyStartActivity.Model.RecurrentNextSteps
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

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        ConcludeEncounter personId encounterId labsResultsId value ->
            ( model
            , Cmd.none
            , [-- @todo
               -- Backend.HealthyStartEncounter.Model.SaveLabsResults personId (Just labsResultsId) { value | resolutionDate = currentDate }
               --        |> Backend.Model.MsgHealthyStartEncounter encounterId
               --        |> App.Model.MsgIndexedDb
              ]
            )
                |> sequenceExtra (update currentDate id) [ SetActivePage <| UserPage GlobalCaseManagementPage ]
