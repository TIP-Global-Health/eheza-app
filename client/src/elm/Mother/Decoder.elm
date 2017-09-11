module Mother.Decoder
    exposing
        ( decodeMother
        )

import Examination.Decoder exposing (decodeExaminationMother)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Mother.Model exposing (..)
import Utils.Json exposing (decodeDate, decodeIntAsString, decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        -- The default avatar comes from SASS , not from the Model.
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string ""
        |> required "children" (oneOf [ list int, decodeNullAsEmptyArray ])
        |> required "examinations" (list decodeExaminationMother)
        |> required "date_birth" decodeDate
