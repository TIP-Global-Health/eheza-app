module Backend.Dashboard.Model exposing (Dashboard, DashboardRaw)

import Backend.HealthCenter.Model exposing (HealthCenter)


{-| The raw info we get for the dashboard.
-}
type alias DashboardRaw =
    { computed : String
    , healthCenter : HealthCenter
    }


{-| The computed info.
-}
type alias Dashboard =
    { computed : String
    , healthCenter : HealthCenter
    }
