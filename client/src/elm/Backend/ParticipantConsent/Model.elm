module Backend.ParticipantConsent.Model exposing (..)

import Backend.Entities exposing (..)
import HtmlParser exposing (Node)
import Translate.Model exposing (Language, TranslationSet)
import User.Model exposing (UserId)


{-| Models the form we show to obtain a consent.
-}
type alias ParticipantForm =
    { title : TranslationSet String
    , body : TranslationSet FormBody
    }


{-| We immediately parse the HTML we get from the backend, (so we don't have to
do it over and over again), but we also hang on to the raw string. That makes
it easier to encode this structure to send it to local storage.
-}
type alias FormBody =
    { raw : String
    , parsed : List Node
    }
