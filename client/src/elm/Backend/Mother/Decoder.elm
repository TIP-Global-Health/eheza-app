module Backend.Mother.Decoder exposing (decodeHivStatus, decodeMother)

import Backend.Mother.Model exposing (..)
import Backend.Patient.Decoder exposing (decodeGender, decodeUbudehe)
import Backend.Patient.Model exposing (Gender(..))
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)


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
        |> optional "date_birth" (nullable decodeYYYYMMDD) Nothing
        -- Is birth date estimated
        |> hardcoded False
        |> optional "relation" decodeChildrenRelation MotherRelation
        |> optional "gender" decodeGender Female
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "education_level" (nullable decodeEducationLevel) Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing


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


decodeHivStatus : Decoder HIVStatus
decodeHivStatus =
    string
        |> andThen
            (\s ->
                case s of
                    "hiv-exposed-infant" ->
                        succeed HIVExposedInfant

                    "negative" ->
                        succeed Negative

                    "negative-dc" ->
                        succeed NegativeDiscordantCouple

                    "positive" ->
                        succeed Positive

                    "unknown" ->
                        succeed Unknown

                    _ ->
                        fail (s ++ " is not a recognized HIVStatus")
            )


decodeMaritalStatus : Decoder MaritalStatus
decodeMaritalStatus =
    string
        |> andThen
            (\status ->
                case status of
                    "divorced" ->
                        succeed Divorced

                    "married" ->
                        succeed Married

                    "single" ->
                        succeed Single

                    "widowed" ->
                        succeed Widowed

                    _ ->
                        fail (status ++ " is not a recognized MaritalStatus")
            )
