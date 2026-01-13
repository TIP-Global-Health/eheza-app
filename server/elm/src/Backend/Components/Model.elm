module Backend.Components.Model exposing (..)

{-| The return value of Backend update functions
-}


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
