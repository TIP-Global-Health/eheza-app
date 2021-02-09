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
    , showEndEncounetrDialog : Bool
    , warningPopupState : Maybe AcuteIllnessDiagnosis
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showEndEncounetrDialog = False
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

    -- The diagnosis that we have right now.
    -- This can be the diagnosis of first encounter, or one of
    -- subsequent encounters, where diagnoses was updated to
    -- Malaria (this is the only case when we allow to update a diagnosis).
    , diagnosis : Maybe ( NominalDate, AcuteIllnessDiagnosis )

    -- In case diagnosis was updated to Malaria, stores the original
    -- diagnosis that was determined at first encounter.
    , previousDiagnosis : Maybe ( NominalDate, AcuteIllnessDiagnosis )
    }


type alias AcuteIllnessEncounterData =
    { startDate : NominalDate
    , sequenceNumber : Int
    , diagnosis : AcuteIllnessDiagnosis
    , measurements : AcuteIllnessMeasurements
    }
