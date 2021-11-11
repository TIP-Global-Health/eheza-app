module Pages.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounterData, AssembledData, Model, Msg(..), Tab(..), emptyModel)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    , showEndEncounterDialog : Bool
    , warningPopupState : Maybe AcuteIllnessDiagnosis
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showEndEncounterDialog = False
    , warningPopupState = Nothing
    }


type Msg
    = CloseEncounter AcuteIllnessEncounterId
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetEndEncounterDialogState Bool
    | SetSelectedTab Tab
    | SetWarningPopupState (Maybe AcuteIllnessDiagnosis)


type Tab
    = Completed
    | Pending
    | Reports


type alias AssembledData =
    { id : AcuteIllnessEncounterId
    , encounter : AcuteIllnessEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : AcuteIllnessMeasurements
    , previousEncountersData : List AcuteIllnessEncounterData

    -- During inital encounter full set of measurements is collected,
    -- It can be executed by both nurse and CHW, while
    -- subsequent encounter is done only by CHW.
    , initialEncounter : Bool

    -- The diagnosis that we have right now.
    -- This can be the diagnosis of first encounter, or one of
    -- subsequent encounters, where diagnoses was updated to
    -- Malaria (this is the only case when we allow to update a diagnosis).
    , diagnosis : Maybe ( NominalDate, AcuteIllnessDiagnosis )
    }


type alias AcuteIllnessEncounterData =
    { id : AcuteIllnessEncounterId
    , startDate : NominalDate
    , sequenceNumber : Int
    , diagnosis : AcuteIllnessDiagnosis
    , measurements : AcuteIllnessMeasurements
    }
