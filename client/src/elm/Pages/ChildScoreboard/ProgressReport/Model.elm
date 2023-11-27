module Pages.ChildScoreboard.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.Page exposing (Page)


type alias Model =
    { showAIEncounterPopup : Bool }


emptyModel : Model
emptyModel =
    { showAIEncounterPopup = False }


type Msg
    = CloseEncounter ChildScoreboardEncounterId
    | SetActivePage Page
    | ShowAIEncounterPopup
    | TriggerAcuteIllnessEncounter AssembledData
