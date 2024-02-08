module Pages.Tuberculosis.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (TuberculosisMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.TuberculosisEncounter.Model exposing (..)
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
    { id : TuberculosisEncounterId
    , encounter : TuberculosisEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : TuberculosisMeasurements
    , previousEncountersData : List PreviousEncounterData
    }


type alias PreviousEncounterData =
    { id : TuberculosisEncounterId
    , startDate : NominalDate
    , measurements : TuberculosisMeasurements
    }


type Msg
    = CloseEncounter TuberculosisEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SetEndEncounterDialogState Bool
