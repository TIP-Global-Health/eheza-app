module Pages.Prenatal.Encounter.Model exposing (AssembledData, Model, Msg(..), Tab(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ObstetricHistoryValue, PrenatalMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    , showWarningForChw : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showWarningForChw = False
    }


type alias AssembledData =
    { id : PrenatalEncounterId
    , encounter : PrenatalEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : PrenatalMeasurements
    , nursePreviousMeasurementsWithDates : List ( NominalDate, PrenatalMeasurements )
    , chwPreviousMeasurementsWithDates : List ( NominalDate, PrenatalEncounterType, PrenatalMeasurements )
    , globalLmpDate : Maybe NominalDate
    , globalObstetricHistory : Maybe ObstetricHistoryValue
    }


type Msg
    = CloseEncounter
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetChwWarningVisible Bool
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports
