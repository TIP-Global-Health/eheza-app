module Pages.Prenatal.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model
    exposing
        ( AvoidingGuidanceReason
        , MedicationNonAdministrationSign
        , NonReferralSign
        , ObstetricHistoryValue
        , PrenatalMeasurements
        , PrenatalVaccineType
        , ReasonForNonReferral
        , RecommendedTreatmentSign
        , VaccineDose
        )
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)


type alias AssembledData =
    { id : PrenatalEncounterId
    , encounter : PrenatalEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : PrenatalMeasurements
    , nursePreviousEncountersData : List PreviousEncounterData
    , chwPreviousMeasurementsWithDates : List ( NominalDate, PrenatalEncounterType, PrenatalMeasurements )
    , globalLmpDate : Maybe NominalDate
    , globalObstetricHistory : Maybe ObstetricHistoryValue
    , vaccinationHistory : VaccinationProgressDict

    -- Similar to vaccinationHistory, but includes immunisation data of current encounter.
    , vaccinationProgress : VaccinationProgressDict
    }


type alias PreviousEncounterData =
    { startDate : NominalDate
    , diagnoses : EverySet PrenatalDiagnosis
    , pastDiagnoses : EverySet PrenatalDiagnosis
    , measurements : PrenatalMeasurements
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
    , vitaminA : Maybe Bool
    , paracetamol : Maybe Bool
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
    , vitaminA = Nothing
    , paracetamol = Nothing
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
    , earlyMastitisOrEngorgment : Maybe Bool
    , mastitis : Maybe Bool
    , grief : Maybe Bool
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
    , earlyMastitisOrEngorgment = Nothing
    , mastitis = Nothing
    , grief = Nothing
    }


type alias ReferralForm =
    { -- Fields used for CHW:
      handReferralForm : Maybe Bool
    , referToHealthCenter : Maybe Bool
    , accompanyToHealthCenter : Maybe Bool
    , reasonForNotSendingToHC : Maybe ReasonForNonReferral

    -- Fields used for Nurse:
    , referToHospital : Maybe Bool
    , referralFormHospital : Maybe Bool
    , referToMentalHealthSpecialist : Maybe Bool
    , referralFormMentalHealthSpecialist : Maybe Bool
    , accompanyToMentalHealthSpecialist : Maybe Bool
    , referToARVProgram : Maybe Bool
    , referralFormARVProgram : Maybe Bool
    , accompanyToARVProgram : Maybe Bool
    , referToNCDProgram : Maybe Bool
    , referralFormNCDProgram : Maybe Bool
    , accompanyToNCDProgram : Maybe Bool
    , referToUltrasound : Maybe Bool
    , referralFormUltrasound : Maybe Bool
    , facilityNonReferralReasons : Maybe (EverySet NonReferralSign)
    }


emptyReferralForm : ReferralForm
emptyReferralForm =
    { handReferralForm = Nothing
    , referToHealthCenter = Nothing
    , accompanyToHealthCenter = Nothing
    , reasonForNotSendingToHC = Nothing
    , referToHospital = Nothing
    , referralFormHospital = Nothing
    , referToMentalHealthSpecialist = Nothing
    , referralFormMentalHealthSpecialist = Nothing
    , accompanyToMentalHealthSpecialist = Nothing
    , referToARVProgram = Nothing
    , referralFormARVProgram = Nothing
    , accompanyToARVProgram = Nothing
    , referToNCDProgram = Nothing
    , referralFormNCDProgram = Nothing
    , accompanyToNCDProgram = Nothing
    , referToUltrasound = Nothing
    , referralFormUltrasound = Nothing
    , facilityNonReferralReasons = Nothing
    }


type PrenatalEncounterPhase
    = PrenatalEncounterPhaseInitial
    | PrenatalEncounterPhaseRecurrent


type HypertensionTreatementUpdateOption
    = TreatementUpdateMaintainCurrentDoasage
    | TreatementUpdateIncreaseOneDose
    | TreatementUpdateIncreaseTwoDoses
    | TreatementUpdateHospitalize


type alias MalariaPreventionData =
    { form : MalariaPreventionForm
    }


emptyMalariaPreventionData : MalariaPreventionData
emptyMalariaPreventionData =
    { form = emptyMalariaPreventionForm
    }


type alias MalariaPreventionForm =
    { receivedMosquitoNet : Maybe Bool
    }


emptyMalariaPreventionForm : MalariaPreventionForm
emptyMalariaPreventionForm =
    MalariaPreventionForm Nothing
