module Pages.Prenatal.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model
    exposing
        ( AvoidingGuidanceReason
        , MedicationNonAdministrationSign
        , ObstetricHistoryValue
        , PrenatalMeasurements
        , PrenatalVaccineType
        , RecommendedTreatmentSign
        , VaccineDose
        )
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
    , vaccinationHistory : VaccinationProgressDict

    -- Similar to vaccinationHistory, but includes immunisation data of current encounter.
    , vaccinationProgress : VaccinationProgressDict
    }


type alias VaccinationProgressDict =
    Dict PrenatalVaccineType (Dict VaccineDose NominalDate)


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
    , hypertensionAvoidingGuidanceReason : Maybe AvoidingGuidanceReason
    , hypertensionAvoidingGuidanceReasonDirty : Bool
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
    , hypertensionAvoidingGuidanceReason = Nothing
    , hypertensionAvoidingGuidanceReasonDirty = False
    }


type alias HealthEducationForm =
    { expectations : Maybe Bool
    , visitsReview : Maybe Bool
    , warningSigns : Maybe Bool
    , hemorrhaging : Maybe Bool
    , familyPlanning : Maybe Bool
    , breastfeeding : Maybe Bool
    , immunization : Maybe Bool
    , hygiene : Maybe Bool
    , positiveHIV : Maybe Bool
    , saferSexHIV : Maybe Bool
    , partnerTesting : Maybe Bool
    , nauseaVomiting : Maybe Bool
    , legCramps : Maybe Bool
    , lowBackPain : Maybe Bool
    , constipation : Maybe Bool
    , heartburn : Maybe Bool
    , varicoseVeins : Maybe Bool
    , legPainRedness : Maybe Bool
    , pelvicPain : Maybe Bool
    , saferSex : Maybe Bool
    , mentalHealth : Maybe Bool
    , hivDetectableViralLoad : Maybe Bool
    , diabetes : Maybe Bool
    }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    { expectations = Nothing
    , visitsReview = Nothing
    , warningSigns = Nothing
    , hemorrhaging = Nothing
    , familyPlanning = Nothing
    , breastfeeding = Nothing
    , immunization = Nothing
    , hygiene = Nothing
    , positiveHIV = Nothing
    , saferSexHIV = Nothing
    , partnerTesting = Nothing
    , nauseaVomiting = Nothing
    , legCramps = Nothing
    , lowBackPain = Nothing
    , constipation = Nothing
    , heartburn = Nothing
    , varicoseVeins = Nothing
    , legPainRedness = Nothing
    , pelvicPain = Nothing
    , saferSex = Nothing
    , mentalHealth = Nothing
    , hivDetectableViralLoad = Nothing
    , diabetes = Nothing
    }


type PrenatalEncounterPhase
    = PrenatalEncounterPhaseInitial
    | PrenatalEncounterPhaseRecurrent


type HypertensionTreatementUpdateOption
    = TreatementUpdateMaintainCurrentDoasage
    | TreatementUpdateIncreaseOneDose
    | TreatementUpdateIncreaseTwoDoses
    | TreatementUpdateHospitalize
