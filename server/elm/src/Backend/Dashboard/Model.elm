module Backend.Dashboard.Model exposing (DashboardData, Msg(..))

import Backend.Components.Model exposing (MenuData)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


{-| What Drupal hands the dashboards: what it hands the report menus, plus
the first cohort's enrolment date when it is set.
-}
type alias DashboardData =
    { firstCohortDate : Maybe NominalDate
    , menu : MenuData
    }


type Msg
    = SetData Value
