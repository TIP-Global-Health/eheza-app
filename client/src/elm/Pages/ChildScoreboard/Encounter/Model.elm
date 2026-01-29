module Pages.ChildScoreboard.Encounter.Model exposing (AssembledData, Model, Msg(..), Tab(..), emptyModel)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (VaccinationProgressDict)
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
    | Scorecard


type alias AssembledData =
    { id : ChildScoreboardEncounterId
    , encounter : ChildScoreboardEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : ChildScoreboardMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( ChildScoreboardEncounterId, ChildScoreboardMeasurements ) )
    , vaccinationHistory : VaccinationProgressDict

    -- Similar to vaccinationHistory, but includes immunisation data of current encounter.
    , vaccinationProgress : VaccinationProgressDict
    }


type Msg
    = CloseEncounter ChildScoreboardEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | ShowAIEncounterPopup
    | TriggerAcuteIllnessEncounter AssembledData
