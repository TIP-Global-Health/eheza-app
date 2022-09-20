module Pages.NCD.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias AssembledData =
    { id : NCDEncounterId
    , encounter : NCDEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : NCDMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( NCDEncounterId, NCDMeasurements ) )
    }


type alias ReferralForm =
    { referToHospital : Maybe Bool
    , referralFormHospital : Maybe Bool
    , referToANCServices : Maybe Bool
    , referralFormANCServices : Maybe Bool
    , accompanyToANCServices : Maybe Bool
    , nonReferralReasons : Maybe (EverySet NonReferralSign)
    }


emptyReferralForm : ReferralForm
emptyReferralForm =
    { referToHospital = Nothing
    , referralFormHospital = Nothing
    , referToANCServices = Nothing
    , referralFormANCServices = Nothing
    , accompanyToANCServices = Nothing
    , nonReferralReasons = Nothing
    }


type alias MedicationDistributionForm =
    { recommendedTreatmentSigns : Maybe (List RecommendedTreatmentSign)
    , guidedToReturnInOneMonth : Maybe Bool
    }


emptyMedicationDistributionForm : MedicationDistributionForm
emptyMedicationDistributionForm =
    { recommendedTreatmentSigns = Nothing
    , guidedToReturnInOneMonth = Nothing
    }
