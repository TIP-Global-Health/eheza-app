module Backend.ParticipantConsent.Decoder exposing (..)

import Backend.ParticipantConsent.Model exposing (..)
import HtmlParser exposing (Node)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Translate.Model exposing (TranslationSet)


decodeParticipantForm : Decoder ParticipantForm
decodeParticipantForm =
    map2 ParticipantForm decodeTitle decodeBody


decodeTitle : Decoder (TranslationSet String)
decodeTitle =
    map2 TranslationSet
        (field "label" string)
        (field "kinyarwanda_title" (nullable string))


decodeBody : Decoder (TranslationSet FormBody)
decodeBody =
    let
        go english kinyarwanda =
            { english = FormBody english (HtmlParser.parse english)
            , kinyarwanda = Maybe.map (\k -> FormBody k (HtmlParser.parse k)) kinyarwanda
            }
    in
    map2 go
        (at [ "body", "value" ] string)
        (at [ "kinyarwanda_body", "value" ] (nullable string))
