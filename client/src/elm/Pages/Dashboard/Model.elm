module Pages.Dashboard.Model exposing
    ( Card
    , CardValueSeverity(..)
    , FamilyPlanningSignsCounter
    , FilterGender(..)
    , FilterPeriod(..)
    , Model
    , Msg(..)
    , emptyModel
    , filterGenders
    , filterPeriods
    )

{-| Filtering by period
-}

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Color exposing (Color)
import Translate exposing (TranslationId)


type FilterPeriod
    = ThisMonth
    | LastMonth
    | ThreeMonths
    | OneYear


filterPeriods : List FilterPeriod
filterPeriods =
    [ ThisMonth
    , LastMonth
    , ThreeMonths
    , OneYear
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
    { period = ThisMonth
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


type Msg
    = SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
