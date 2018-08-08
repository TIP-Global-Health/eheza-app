module Backend.Mother.Decoder
    exposing
        ( decodeMother
        )

import Backend.Mother.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityId)
import Utils.Json exposing (decodeDate, decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        -- We accommodate the JSON from the server or from the cache
        |> custom
            (oneOf
                [ map Just <| at [ "avatar", "styles", "patient-photo" ] string
                , map Just <| field "avatar" string
                , succeed Nothing
                ]
            )
        |> required "children" (oneOf [ list decodeEntityId, decodeNullAsEmptyArray ])
        |> required "date_birth" decodeYYYYMMDD
        |> optional "ubudehe" (nullable string) Nothing
        |> required "education_level" decodeEducationLevel


decodeEducationLevel : Decoder EducationLevel
decodeEducationLevel =
    string
        |> andThen
            (\educationLevel ->
                case educationLevel of
                    "0" ->
                        succeed NoSchooling

                    "1" ->
                        succeed PrimarySchool

                    "2" ->
                        succeed VocationalTrainingSchool

                    "3" ->
                        succeed SecondarySchool

                    "4" ->
                        succeed DiplomaProgram

                    "5" ->
                        succeed HigherEducation

                    _ ->
                        fail <|
                            educationLevel
                                ++ " is not a recognized EducationLevel"
            )
