module Backend.Mother.Decoder exposing (decodeMother)

import Backend.Mother.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)


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
        |> optional "date_birth" (map Just decodeYYYYMMDD) Nothing
        |> optional "relation" decodeChildrenRelation MotherRelation
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "education_level" (nullable decodeEducationLevel) Nothing


decodeChildrenRelation : Decoder ChildrenRelationType
decodeChildrenRelation =
    string
        |> andThen
            (\relation ->
                case relation of
                    "mother" ->
                        succeed MotherRelation

                    "caregiver" ->
                        succeed CaregiverRelation

                    _ ->
                        fail <| relation ++ " is not a recognized ChildrenRelationType"
            )


decodeUbudehe : Decoder Ubudehe
decodeUbudehe =
    decodeInt
        |> andThen
            (\ubudehe ->
                case ubudehe of
                    1 ->
                        succeed Ubudehe1

                    2 ->
                        succeed Ubudehe2

                    3 ->
                        succeed Ubudehe3

                    4 ->
                        succeed Ubudehe4

                    _ ->
                        fail <| toString ubudehe ++ " is out of range for Ubudehe"
            )


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
