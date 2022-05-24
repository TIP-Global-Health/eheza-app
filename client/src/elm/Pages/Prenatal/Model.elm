module Pages.Prenatal.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (MedicationNonAdministrationSign, ObstetricHistoryValue, PrenatalMeasurements, RecommendedTreatmentSign)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)


type alias AssembledData =
    { id : PrenatalEncounterId
    , encounter : PrenatalEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : PrenatalMeasurements
    , nursePreviousMeasurementsWithDates : List ( NominalDate, EverySet PrenatalDiagnosis, PrenatalMeasurements )
    , chwPreviousMeasurementsWithDates : List ( NominalDate, PrenatalEncounterType, PrenatalMeasurements )
    , globalLmpDate : Maybe NominalDate
    , globalObstetricHistory : Maybe ObstetricHistoryValue
    }


type alias MedicationDistributionForm =
    { mebendezole : Maybe Bool

    -- Tenofovir is not in use, for now.
    , tenofovir : Maybe Bool

    -- Lamivudine is not in use, for now.
    , lamivudine : Maybe Bool
    , dolutegravir : Maybe Bool
    , tdf3tc : Maybe Bool
    , iron : Maybe Bool
    , folicAcid : Maybe Bool
    , ceftriaxone : Maybe Bool
    , azithromycin : Maybe Bool
    , metronidazole : Maybe Bool
    , nonAdministrationSigns : Maybe (EverySet MedicationNonAdministrationSign)
    , recommendedTreatmentSigns : Maybe (List RecommendedTreatmentSign)
    }


emptyMedicationDistributionForm : MedicationDistributionForm
emptyMedicationDistributionForm =
    { mebendezole = Nothing
    , tenofovir = Nothing
    , lamivudine = Nothing
    , dolutegravir = Nothing
    , tdf3tc = Nothing
    , iron = Nothing
    , folicAcid = Nothing
    , ceftriaxone = Nothing
    , azithromycin = Nothing
    , metronidazole = Nothing
    , nonAdministrationSigns = Nothing
    , recommendedTreatmentSigns = Nothing
    }


type PrenatalEncounterPhase
    = PrenatalEncounterPhaseInitial
    | PrenatalEncounterPhaseRecurrent


type HypertensionTreatementUpdateOption
    = TreatementUpdateMaintainCurrentDoasage
    | TreatementUpdateIncreaseOneDose
    | TreatementUpdateIncreaseTwoDoses
    | TreatementUpdateHospitalize
