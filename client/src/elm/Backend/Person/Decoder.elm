module Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodePerson, decodeUbudehe)

import Backend.Person.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodePerson : Decoder Person
decodePerson =
    decode Person
        |> required "label" string
        |> optional "first_name" string ""
        |> optional "middle_name" (nullable string) Nothing
        |> optional "second_name" string ""
        |> optional "national_id_number" (nullable string) Nothing
        |> optional "photo" (nullable string) Nothing
        |> optional "birth_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "birth_date_estimated" bool False
        |> optional "gender" decodeGender Female
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "education_level" (nullable decodeEducationLevel) Nothing
        |> optional "profession" (nullable string) Nothing
        |> optional "marital_status" (nullable decodeMaritalStatus) Nothing
        |> optional "province" (nullable string) Nothing
        |> optional "district" (nullable string) Nothing
        |> optional "sector" (nullable string) Nothing
        |> optional "cell" (nullable string) Nothing
        |> optional "village" (nullable string) Nothing
        |> optional "phone_number" (nullable string) Nothing


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\gender ->
                if gender == "female" then
                    succeed Female

                else if gender == "male" then
                    succeed Male

                else
                    fail (gender ++ " is not a recognized 'type' for Gender.")
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
