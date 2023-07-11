module Pages.ChildScoreboard.Encounter.Model exposing (..)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, NutritionSupplementType)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAIEncounterPopup : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAIEncounterPopup = False
    }


type Tab
    = Completed
    | Pending


type alias AssembledData =
    { id : ChildScoreboardEncounterId
    , encounter : ChildScoreboardEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : ChildScoreboardMeasurements
    }


type Msg
    = CloseEncounter ChildScoreboardEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | ShowAIEncounterPopup
    | TriggerAcuteIllnessEncounter AssembledData
