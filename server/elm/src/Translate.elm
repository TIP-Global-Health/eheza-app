module Translate exposing
    ( StringIdHttpError(..)
    , TranslationId(..)
    , translate
    )

import App.Types exposing (Language(..))
import Pages.Scoreboard.Model exposing (SelectedEntity(..))


{-| Main function to call for translation.
-}
translate : Language -> TranslationId -> String
translate language transId =
    let
        set =
            translationSet transId
    in
    case language of
        English ->
            .english set

        Kinyarwanda ->
            .kinyarwanda set
                |> Maybe.withDefault (.english set)


type alias TranslationSet =
    { english : String
    , kinyarwanda : Maybe String
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
    | SelectedEntity SelectedEntity
    | Village


translationSet : TranslationId -> TranslationSet
translationSet transId =
    case transId of
        AggregatedChildScoreboard ->
            { english = "Aggregated Child Scoreboard"
            , kinyarwanda = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            }

        District ->
            { english = "District"
            , kinyarwanda = Nothing
            }

        GenerateReport ->
            { english = "Generate Report"
            , kinyarwanda = Nothing
            }

        HttpError val ->
            translateHttpError val

        Province ->
            { english = "Province"
            , kinyarwanda = Nothing
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Nothing
            }

        SelectedEntity entity ->
            case entity of
                EntityDistrict ->
                    translationSet District

                EntitySector ->
                    translationSet Sector

                EntityCell ->
                    translationSet Cell

                EntityVillage ->
                    translationSet Village

        Village ->
            { english = "Village"
            , kinyarwanda = Nothing
            }


translateHttpError : StringIdHttpError -> TranslationSet
translateHttpError transId =
    case transId of
        ErrorBadUrl ->
            { english = "URL is not valid."
            , kinyarwanda = Nothing
            }

        ErrorBadPayload message ->
            { english = "The server responded with data of an unexpected type: " ++ message
            , kinyarwanda = Nothing
            }

        ErrorBadStatus err ->
            { english = err
            , kinyarwanda = Nothing
            }

        ErrorNetworkError ->
            { english = "There was a network error."
            , kinyarwanda = Nothing
            }

        ErrorTimeout ->
            { english = "The network request timed out."
            , kinyarwanda = Nothing
            }
