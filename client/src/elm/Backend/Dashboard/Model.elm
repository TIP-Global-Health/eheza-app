module Backend.Dashboard.Model exposing (DashboardStats, PeopleStats, emptyModel)

{-| The stats for the dashboard.
-}

import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)
import Time


type alias DashboardStats =
    { people : List PeopleStats
    }


emptyModel : DashboardStats
emptyModel =
    { people = []
    }


type alias PeopleStats =
    { gender : Gender
    , birthdate : NominalDate
    , memberSince : NominalDate
    }
