module Pages.HIV.Encounter.Model exposing (AssembledData, EncounterData, Model, Msg(..), Tab(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model exposing (HIVEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (HIVMeasurements)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showEndEncounterDialog = False
    }


type Tab
    = Completed
    | Pending
    | Reports


type alias AssembledData =
    { id : HIVEncounterId
    , encounter : HIVEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : HIVMeasurements
    , previousEncountersData : List EncounterData

    -- Intial encounter is the one where initial diagnosis is made.
    , initialEncounter : Bool
    }


type alias EncounterData =
    { id : HIVEncounterId
    , startDate : NominalDate
    , measurements : HIVMeasurements
    }


type Msg
    = CloseEncounter HIVEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SetEndEncounterDialogState Bool
