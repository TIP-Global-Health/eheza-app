module Pages.Dashboard.Model exposing
    ( Card
    , CardValueSeverity(..)
    , FamilyPlanningSignsCounter
    , FilterGender(..)
    , FilterPeriod(..)
    , Model
    , Msg(..)
    , StatsCard
    , emptyModel
    , filterGenders
    , filterPeriods
    )

{-| Filtering by period
-}

import AssocList exposing (Dict)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Pages.Page exposing (DashboardPage, Page)
import Translate exposing (TranslationId)


type FilterPeriod
    = OneYear


filterPeriods : List FilterPeriod
filterPeriods =
    [ OneYear
    ]


{-| We don't use the existing `Gender`, as we want to have an `All` option as-well
-}
type FilterGender
    = All
    | Female
    | Male


filterGenders : List FilterGender
filterGenders =
    [ All
    , Female
    , Male
    ]


type alias Model =
    { period : FilterPeriod
    , beneficiariesGender : FilterGender
    }


emptyModel : Model
emptyModel =
    { period = OneYear
    , beneficiariesGender = All
    }


{-| A record to hold the count of total signs used.
-}
type alias FamilyPlanningSignsCounter =
    Dict FamilyPlanningSign Int


{-| This will define what color the `value` from a `Card` will appear in.
-}
type CardValueSeverity
    = Neutral
    | Good
    | Moderate
    | Severe


{-| A `Card` that will appear in the dashboard.
-}
type alias Card =
    { title : TranslationId
    , value : Int
    , valueSeverity : CardValueSeverity
    }


{-| A `Stat Card` that will appear in the dashboard and display a certain statistic with difference from last year.
-}
type alias StatsCard =
    { title : TranslationId
    , cardClasses : String
    , cardAction : Maybe DashboardPage
    , value : Int
    , valueSeverity : CardValueSeverity
    , valueIsPercentage : Bool
    , percentageLastYear : Int
    , newCases : Maybe Int
    }


type Msg
    = SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
    | SetActivePage Page
