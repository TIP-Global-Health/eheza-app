module Pages.HomeVisit.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (HomeVisitMeasurements)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    }


type Msg
    = CloseEncounter HomeVisitEncounterId
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
    { id : HomeVisitEncounterId
    , encounter : HomeVisitEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : HomeVisitMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( HomeVisitEncounterId, HomeVisitMeasurements ) )
    }
