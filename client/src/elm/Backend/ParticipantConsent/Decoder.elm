module Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)

import Backend.ParticipantConsent.Model exposing (..)
import Html.Parser as HtmlParser
import Json.Decode exposing (..)
import Translate.Model exposing (TranslationSet)


decodeParticipantForm : Decoder ParticipantForm
decodeParticipantForm =
    map2 ParticipantForm decodeTitle decodeBody


decodeTitle : Decoder (TranslationSet String)
decodeTitle =
    map3 TranslationSet
        (field "label" string)
        (field "kinyarwanda_title" (nullable string))
        (succeed Nothing)


decodeBody : Decoder (TranslationSet FormBody)
decodeBody =
    let
        parse v =
            HtmlParser.run v
                |> Result.toMaybe
                |> Maybe.withDefault []

        go english kinyarwanda =
            { english = FormBody english (parse english)
            , kinyarwanda = Maybe.map (\k -> FormBody k (parse k)) kinyarwanda
            , kirundi = Nothing
            }
    in
    map2 go
        (field "body" string)
        (field "kinyarwanda_body" (nullable string))
