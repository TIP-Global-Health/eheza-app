module Pages.Model exposing (MetricsResultsTableData)


type alias MetricsResultsTableData =
    { heading : String
    , captions : List String
    , rows : List (List String)
    }
