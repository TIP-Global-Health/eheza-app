module Backend.Reports.Model exposing (..)

import App.Types exposing (Site)
import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias ReportsData =
    { site : Site
    , entityName : String
    , entityType : SelectedEntity
    , records : List PatientData
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
    , prenatalData : Maybe (List (List PrenatalEncounterData))
    , homeVisitData : Maybe (List (List HomeVisitEncounterData))
    , wellChildData : Maybe (List (List NutritionEncounterData))
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
    }


type AcuteIllnessEncounterType
    = AcuteIllnessEncounterNurse
    | AcuteIllnessEncounterNurseSubsequent
    | AcuteIllnessEncounterCHW


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


type Msg
    = SetData Value
