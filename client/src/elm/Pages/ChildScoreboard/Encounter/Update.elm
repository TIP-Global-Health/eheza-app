module Pages.ChildScoreboard.Encounter.Update exposing (closeEncounterMsgs, update)

import App.Model
import Backend.ChildScoreboardEncounter.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model
import Pages.ChildScoreboard.Encounter.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , closeEncounterMsgs id ++ [ App.Model.SetActivePage PinCodePage ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        ShowAIEncounterPopup id ->
            -- Ending the encounter is what the button asked for, so it
            -- happens now. The popup that follows is the referral, not a
            -- second chance to decide.
            ( { model | showAIEncounterPopup = True }
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
