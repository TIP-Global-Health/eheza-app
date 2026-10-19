module Backend.Dashboard.Model exposing (DashboardData, Msg(..))

import Backend.Components.Model exposing (HealthCenterData)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


{-| What Drupal hands the dashboards: the health centers, and the first
cohort's enrolment date when it is set.
-}
type alias DashboardData =
    { firstCohortDate : Maybe NominalDate
    , healthCenters : List HealthCenterData
    }


type Msg
    = SetData Value
