module Pages.WellChildEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (WellChildMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    }


type Msg
    = CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }


type alias AssembledData =
    { id : WellChildEncounterId
    , encounter : WellChildEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : WellChildMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    }
