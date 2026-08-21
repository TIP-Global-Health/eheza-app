module Pages.ChildScoreboard.ProgressReport.Update exposing (update)

import App.Model
import Backend.ChildScoreboardEncounter.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.ChildScoreboard.ProgressReport.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        CloseEncounter id ->
            -- The dialog is cleared as well: this page model is kept per
            -- encounter and outlives the encounter, so a flag left set here
            -- greets whoever opens the report next.
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , closeEncounterMsgs id ++ [ App.Model.SetActivePage PinCodePage ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        ShowAIEncounterPopup id ->
            -- The encounter is closed here, because that is what was just
            -- confirmed. The popup that follows is the referral, not a second
            -- chance to decide, and the dialog goes so the two do not stack.
            ( { model | showAIEncounterPopup = True, showEndEncounterDialog = False }
            , Cmd.none
            , closeEncounterMsgs id
            )

        TriggerAcuteIllnessEncounter assembled ->
            -- The encounter was closed when the popup opened, so this only
            -- navigates. Going through CloseEncounter would append its own
            -- SetActivePage PinCodePage after this one, and the last one wins.
            ( { model | showAIEncounterPopup = False }
            , Cmd.none
            , [ App.Model.SetActivePage <| UserPage (AcuteIllnessParticipantPage InitiatorParticipantsPage assembled.participant.person) ]
            )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )

        MsgReportToWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, cmd, ( extraMsgs, appMsgs ) ) =
                    Components.ReportToWhatsAppDialog.Update.update subMsg model.reportToWhatsAppDialog
            in
            ( { model | reportToWhatsAppDialog = dialogUpdated }, cmd, appMsgs )
                |> sequenceExtra update extraMsgs

        SetReportComponents maybeComponents ->
            let
                updatedModel =
                    Maybe.map
                        (\components ->
                            case components of
                                Components.ReportToWhatsAppDialog.Model.WellChild wellChildComponents ->
                                    { model | components = Just wellChildComponents }

                                -- We should never get here.
                                _ ->
                                    model
                        )
                        maybeComponents
                        |> Maybe.withDefault { model | components = Nothing }
            in
            ( updatedModel, Cmd.none, [] )

        SetReportTab tab ->
            ( { model | reportTab = tab }, Cmd.none, [] )


{-| Closing the encounter, without saying where to go next. The two callers
disagree about that: ending normally returns to the PIN code page, while a
referral carries on to the acute illness encounter.
-}
closeEncounterMsgs : ChildScoreboardEncounterId -> List App.Model.Msg
closeEncounterMsgs id =
    [ Backend.ChildScoreboardEncounter.Model.CloseChildScoreboardEncounter
        |> Backend.Model.MsgChildScoreboardEncounter id
        |> App.Model.MsgIndexedDb
    ]
