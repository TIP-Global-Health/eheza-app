module Pages.ChildScoreboard.Report.Update exposing (update)

import App.Model
import Backend.ChildScoreboardEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model
import Gizra.Update exposing (sequenceExtra)
import Pages.ChildScoreboard.Report.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.ChildScoreboardEncounter.Model.CloseChildScoreboardEncounter
                    |> Backend.Model.MsgChildScoreboardEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        ShowAIEncounterPopup ->
            ( { model | showAIEncounterPopup = True }
            , Cmd.none
            , []
            )

        TriggerAcuteIllnessEncounter assembled ->
            ( { model | showAIEncounterPopup = False }
            , Cmd.none
            , [ App.Model.SetActivePage <| UserPage (AcuteIllnessParticipantPage InitiatorParticipantsPage assembled.participant.person) ]
            )
                |> sequenceExtra update [ CloseEncounter assembled.id ]
