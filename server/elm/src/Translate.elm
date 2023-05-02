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
    = HttpError StringIdHttpError


{-| Main function to call for translation.
-}
translate : Language -> TranslationId -> String
translate language trans =
    let
        translationSet =
            case trans of
                HttpError val ->
                    translateHttpError val
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
