module Pages.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AssembledData, Model, Msg(..), Tab(..), emptyModel)

-- import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)

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
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showEndEncounetrDialog = False
    }


type Msg
    = CloseEncounter AcuteIllnessEncounterId
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetEndEncounterDialogState Bool
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


type alias AssembledData =
    { id : AcuteIllnessEncounterId
    , encounter : AcuteIllnessEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : AcuteIllnessMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, AcuteIllnessMeasurements )
    }


type AcuteIllnessDiagnosis
    = DiagnosisCovid19
    | DiagnosisMalariaComplicated
    | DiagnosisMalariaUncomplicated
    | DiagnosisGastrointestinalInfectionComplicated
    | DiagnosisGastrointestinalInfectionUncomplicated
    | DiagnosisSimlpeColdAndCough
