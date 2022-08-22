module Pages.NCD.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (NCDMeasurements)
import Backend.NCDEncounter.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showEndEncounterDialog : Bool
    }


type Msg
    = CloseEncounter NCDEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SetEndEncounterDialogState Bool


type Tab
    = Completed
    | Pending
    | Reports


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showEndEncounterDialog = False
    }


type alias AssembledData =
    { id : NCDEncounterId
    , encounter : NCDEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : NCDMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( NCDEncounterId, NCDMeasurements ) )
    }
