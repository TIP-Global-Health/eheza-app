module Translate exposing
    ( StringIdHttpError(..)
    , TranslationId(..)
    , translate
    )

import App.Types exposing (Language(..))


type alias TranslationSet =
    { english : String
    }


type StringIdHttpError
    = ErrorBadUrl
    | ErrorBadPayload String
    | ErrorBadStatus String
    | ErrorNetworkError
    | ErrorTimeout


type TranslationId
    = AggregatedChildScoreboard
    | Cell
    | District
    | GenerateReport
    | HttpError StringIdHttpError
    | Province
    | Sector
    | Village


{-| Main function to call for translation.
-}
translate : Language -> TranslationId -> String
translate language trans =
    let
        translationSet =
            case trans of
                AggregatedChildScoreboard ->
                    { english = "Aggregated Child Scoreboard"
                    }

                Cell ->
                    { english = "Cell"
                    }

                District ->
                    { english = "District"
                    }

                GenerateReport ->
                    { english = "Generate Report"
                    }

                HttpError val ->
                    translateHttpError val

                Province ->
                    { english = "Province"
                    }

                Sector ->
                    { english = "Sector"
                    }

                Village ->
                    { english = "Village" }
    in
    case language of
        English ->
            .english translationSet


translateHttpError : StringIdHttpError -> TranslationSet
translateHttpError transId =
    case transId of
        ErrorBadUrl ->
            { english = "URL is not valid."
            }

        ErrorBadPayload message ->
            { english = "The server responded with data of an unexpected type: " ++ message
            }

        ErrorBadStatus err ->
            { english = err
            }

        ErrorNetworkError ->
            { english = "There was a network error."
            }

        ErrorTimeout ->
            { english = "The network request timed out."
            }
