module Backend.Components.Model exposing (HealthCenterData, HealthCenterId, MenuData, MenuScope(..), PersonId, ReportParams, SelectedEntity(..), SyncResponse)

{-| The return value of Backend update functions
-}

import App.Types exposing (Site)


type alias HealthCenterData =
    { id : HealthCenterId
    , name : String
    }


type alias PersonId =
    Int


type alias HealthCenterId =
    Int


type MenuScope
    = ScopeFull
    | ScopeHealthCenters


type alias MenuData =
    { site : Site
    , healthCenters : List HealthCenterData
    , scope : Maybe MenuScope
    }


type SelectedEntity
    = EntityGlobal
    | EntityProvince
    | EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage
    | EntityHealthCenter


type alias ReportParams =
    { province : Maybe String
    , district : Maybe String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
    , healthCenter : Maybe HealthCenterId
    }


type alias SyncResponse record =
    { records : List record
    , totalRemaining : Int
    , lastIdSynced : PersonId
    }
