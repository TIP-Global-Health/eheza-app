module Pages.Model exposing (..)


type alias MetricsResultsTableData =
    { heading : String
    , captions : List String
    , rows : List (List String)
    }
