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
    { created : NominalDate
    , birthDate : NominalDate
    , gender : Gender
    , acuteIllnessData : Maybe (List (List AcuteIllnessEncounterData))
    , prenatalData : Maybe (List (List PrenatalEncounterData))
    , homeVisitData : Maybe (List EncountersData)
    , wellChildData : Maybe (List EncountersData)
    , individualNutritionData : Maybe (List EncountersData)
    , groupNutritionPmtctData : Maybe EncountersData
    , groupNutritionFbfData : Maybe EncountersData
    , groupNutritionSorwatheData : Maybe EncountersData
    , groupNutritionChwData : Maybe EncountersData
    , groupNutritionAchiData : Maybe EncountersData
    }


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


type alias EncountersData =
    List NominalDate


type Msg
    = SetData Value
