module Pages.Dashboard.Model exposing
    ( FilterGender(..)
    , FilterPeriod(..)
    , Model
    , Msg(..)
    , emptyModel
    , filterGenders
    , filterPeriods
    )

{-| Filtering by period
-}


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


type Msg
    = SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
