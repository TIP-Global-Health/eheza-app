module Backend.Mother.Decoder exposing (decodeMother)

import Backend.Mother.Model exposing (..)
import Backend.Patient.Decoder exposing (decodeGender, decodeUbudehe)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityId)
import Utils.Json exposing (decodeDate, decodeNullAsEmptyArray)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        -- There 3 are first, middle and second names.
        -- We do not pull actual values from server yet.
        |> hardcoded ""
        |> hardcoded Nothing
        |> hardcoded ""
        -- National ID Number
        |> hardcoded Nothing
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
        -- Is birth date estimated
        |> hardcoded False
        |> required "gender" decodeGender
        |> optional "education_level" (nullable decodeEducationLevel) Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing


decodeEducationLevel : Decoder EducationLevel
decodeEducationLevel =
    decodeInt
        |> andThen
            (\educationLevel ->
                case educationLevel of
                    0 ->
                        succeed NoSchooling

                    1 ->
                        succeed PrimarySchool

                    2 ->
                        succeed VocationalTrainingSchool

                    3 ->
                        succeed SecondarySchool

                    4 ->
                        succeed DiplomaProgram

                    5 ->
                        succeed HigherEducation

                    6 ->
                        succeed AdvancedDiploma

                    _ ->
                        fail <|
                            toString educationLevel
                                ++ " is not a recognized EducationLevel"
            )
