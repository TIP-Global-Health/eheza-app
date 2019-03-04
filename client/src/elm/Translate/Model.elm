module Translate.Model exposing (Language(..), TranslationSet, allLanguages)

{-| This exposes the types used for translation.

Actual translations can be found in `Translate.elm`

-}


type Language
    = English
    | Kinyarwanda


allLanguages : List Language
allLanguages =
    [ English
    , Kinyarwanda
    ]


type alias TranslationSet a =
    { english : a
    , kinyarwanda : Maybe a
    }
