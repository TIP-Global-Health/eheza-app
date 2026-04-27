module Pages.AcuteIllness.Encounter.Model exposing (AcuteIllnessEncounterData, AssembledData, Model, Msg(..), Tab(..), emptyModel)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis, AcuteIllnessEncounterType)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
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
    , firstInitialWithSubsequent : List AcuteIllnessEncounterData
    , secondInitialWithSubsequent : List AcuteIllnessEncounterData

    -- Intial encounter is the one where all measurements are taken and
    -- initial diagnosis is made.
    -- For CHW, it's only the first encounter at illness.
    -- For nurse, it's always an initial encounter, since
    -- nurse can do only single encouter throughout the illness.
    , initialEncounter : Bool

    -- The diagnosis that we have right now.
    -- This can be the diagnosis of initial encounter, or one of
    -- subsequent encounters, where diagnoses was updated to
    -- Malaria (this is the only case when we allow to update a diagnosis).
    , diagnosis : Maybe ( NominalDate, AcuteIllnessDiagnosis )
    }


type alias AcuteIllnessEncounterData =
    { id : AcuteIllnessEncounterId
    , encounterType : AcuteIllnessEncounterType
    , startDate : NominalDate
    , sequenceNumber : Int
    , diagnosis : AcuteIllnessDiagnosis
    , measurements : AcuteIllnessMeasurements
    }
