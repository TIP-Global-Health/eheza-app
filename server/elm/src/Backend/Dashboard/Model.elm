module Backend.Dashboard.Model exposing
    ( ChildRecord
    , DashboardData
    , DeliveryLocation(..)
    , Msg(..)
    , PregnancyOutcome(..)
    , PregnancyRecord
    , PrenatalIndicator(..)
    , PrenatalVisit
    )

import Backend.Components.Model exposing (HealthCenterId, MenuData, SyncResponse)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)


{-| What Drupal hands the report menus, plus the first cohort's enrolment
date. `records` fills in batches from `/api/reports-data` after the page loads.
-}
type alias DashboardData =
    { firstCohortDate : Maybe NominalDate
    , menu : MenuData
    , records : List PregnancyRecord
    , remainingForDownload : Maybe Int
    }


{-| The dashboard record of one antenatal pregnancy, as
`docs/healthy-start-dashboards.md` describes it. Only the keys the dashboards
read are decoded.
-}
type alias PregnancyRecord =
    { healthCenter : HealthCenterId
    , edd : Maybe NominalDate
    , dateConcluded : Maybe NominalDate
    , outcome : Maybe PregnancyOutcome
    , deliveryLocation : Maybe DeliveryLocation
    , visits : List PrenatalVisit
    , ultrasounds : List NominalDate
    , highRisk : Bool
    , preEclampsiaRisk : Bool
    , child : Maybe ChildRecord
    }


type PregnancyOutcome
    = OutcomeAbortion
    | OutcomeLiveAtTerm
    | OutcomeLivePreterm
    | OutcomeStillbirthAtTerm
    | OutcomeStillbirthPreterm


type DeliveryLocation
    = FacilityDelivery
    | HomeDelivery


{-| A prenatal encounter. `antenatal` is False for the postpartum encounters.
-}
type alias PrenatalVisit =
    { date : NominalDate
    , antenatal : Bool
    , indicators : List PrenatalIndicator
    }


{-| The indicator codes of a visit that the dashboards read.
-}
type PrenatalIndicator
    = IndicatorAspirin
    | IndicatorCalcium
    | IndicatorGwgAdequate
    | IndicatorGwgInadequate


{-| The newborn registered on the pregnancy, with the dates of its routine
care visits. `birthWeight` is in grams; `birthWeightDate` is the date of the
exam that recorded it.
-}
type alias ChildRecord =
    { birthDate : Maybe NominalDate
    , birthWeight : Maybe Int
    , birthWeightDate : Maybe NominalDate
    , visits : List NominalDate
    }


type Msg
    = HandleSyncResponse (WebData (SyncResponse PregnancyRecord))
    | SendSyncRequest Int
    | SetData Value
