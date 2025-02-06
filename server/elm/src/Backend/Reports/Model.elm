module Backend.Reports.Model exposing (..)

import App.Types exposing (Site)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias ReportsData =
    { site : Site
    , entityName : String
    , entityType : SelectedEntity
    , records : List PatientData
    , nutritionReportData : Maybe (List BackendGeneratedNutritionReportTableDate)
    }


type SelectedEntity
    = EntityGlobal
    | EntityProvince
    | EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage
    | EntityHealthCenter


type alias PatientData =
    { id : PersonId
    , created : NominalDate
    , birthDate : NominalDate
    , gender : Gender
    , acuteIllnessData : Maybe (List (List AcuteIllnessEncounterData))
    , prenatalData : Maybe (List PrenatalParticipantData)
    , homeVisitData : Maybe (List (List HomeVisitEncounterData))
    , wellChildData : Maybe (List (List NutritionEncounterData))
    , childScorecardData : Maybe (List (List ChildScorecardEncounterData))
    , ncdData : Maybe (List (List NCDEncounterData))
    , hivData : Maybe (List (List HIVEncounterData))
    , tuberculosisData : Maybe (List (List TuberculosisEncounterData))
    , individualNutritionData : Maybe (List (List NutritionEncounterData))
    , groupNutritionPmtctData : Maybe (List NutritionEncounterData)
    , groupNutritionFbfData : Maybe (List NutritionEncounterData)
    , groupNutritionSorwatheData : Maybe (List NutritionEncounterData)
    , groupNutritionChwData : Maybe (List NutritionEncounterData)
    , groupNutritionAchiData : Maybe (List NutritionEncounterData)
    }


type alias PersonId =
    Int


type Gender
    = Female
    | Male


type alias AcuteIllnessEncounterData =
    { startDate : NominalDate
    , encounterType : AcuteIllnessEncounterType
    , diagnosis : Maybe AcuteIllnessDiagnosis
    }


type AcuteIllnessEncounterType
    = AcuteIllnessEncounterNurse
    | AcuteIllnessEncounterNurseSubsequent
    | AcuteIllnessEncounterCHW


type AcuteIllnessDiagnosis
    = DiagnosisCovid19Suspect
    | DiagnosisSevereCovid19
    | DiagnosisPneuminialCovid19
    | DiagnosisLowRiskCovid19
    | DiagnosisMalariaComplicated
    | DiagnosisMalariaUncomplicated
    | DiagnosisMalariaUncomplicatedAndPregnant
    | DiagnosisGastrointestinalInfectionComplicated
    | DiagnosisGastrointestinalInfectionUncomplicated
    | DiagnosisSimpleColdAndCough
    | DiagnosisRespiratoryInfectionComplicated
    | DiagnosisRespiratoryInfectionUncomplicated
    | DiagnosisFeverOfUnknownOrigin
    | DiagnosisUndeterminedMoreEvaluationNeeded
    | DiagnosisTuberculosisSuspect


type alias PrenatalParticipantData =
    { created : NominalDate
    , eddDate : Maybe NominalDate
    , dateConcluded : Maybe NominalDate
    , encounters : List PrenatalEncounterData
    }


type alias PrenatalEncounterData =
    { startDate : NominalDate
    , encounterType : PrenatalEncounterType
    }


type PrenatalEncounterType
    = NurseEncounter
    | NursePostpartumEncounter
    | ChwFirstEncounter
    | ChwSecondEncounter
    | ChwThirdPlusEncounter
    | ChwPostpartumEncounter


type alias NutritionEncounterData =
    { startDate : NominalDate
    , nutritionData : Maybe NutritionData
    }


type alias NutritionData =
    { stunting : Maybe Float
    , wasting : Maybe Float
    , underweight : Maybe Float
    }


type alias HomeVisitEncounterData =
    NominalDate


type alias ChildScorecardEncounterData =
    NominalDate


type alias NCDEncounterData =
    NominalDate


type alias HIVEncounterData =
    NominalDate


type alias TuberculosisEncounterData =
    NominalDate


type alias BackendGeneratedNutritionReportTableDate =
    { tableType : NutritionReportTableType
    , captions : List String
    , stuntingModerate : List String
    , stuntingSevere : List String
    , wastingModerate : List String
    , wastingSevere : List String
    , underweightModerate : List String
    , underweightSevere : List String
    }


type NutritionReportTableType
    = NutritionTablePrevalanceOneOrMore
    | NutritionTablePrevalanceTwoOrMore
    | NutritionTableIncidenceMonthOneOrMore
    | NutritionTableIncidenceMonthTwoOrMore
    | NutritionTableIncidenceQuarterOneOrMore
    | NutritionTableIncidenceQuarterTwoOrMore
    | NutritionTableIncidenceYearOneOrMore
    | NutritionTableIncidenceYearTwoOrMore


type Msg
    = SetData Value
