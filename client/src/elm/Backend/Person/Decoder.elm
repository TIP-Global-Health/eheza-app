module Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeHivStatus, decodeMaritalStatus, decodeModeOfDelivery, decodePerson, decodeUbudehe)

import Backend.Person.Model exposing (..)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)
import String.Extra exposing (toTitleCase)


decodePerson : Decoder Person
decodePerson =
    succeed Person
        |> required "label" string
        |> optional "first_name" string ""
        |> optional "second_name" string ""
        |> optional "national_id_number" (nullable string) Nothing
        |> optional "hmis_number" (nullable string) Nothing
        |> optional "photo" (nullable string) Nothing
        |> optional "birth_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "birth_date_estimated" bool False
        |> optional "gender" decodeGender Female
        |> optional "hiv_status" (nullable decodeHivStatus) Nothing
        |> optional "number_of_children" (nullable decodeInt) Nothing
        |> optional "mode_of_delivery" (nullable decodeModeOfDelivery) Nothing
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "education_level" (nullable decodeEducationLevel) Nothing
        |> optional "marital_status" (nullable decodeMaritalStatus) Nothing
        |> optional "province" (nullable decodeGeoField) Nothing
        |> optional "district" (nullable decodeGeoField) Nothing
        |> optional "sector" (nullable decodeGeoField) Nothing
        |> optional "cell" (nullable decodeGeoField) Nothing
        |> optional "village" (nullable decodeGeoField) Nothing
        |> optional "phone_number" (nullable string) Nothing
        |> optional "health_center" (nullable decodeEntityUuid) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing


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


decodeModeOfDelivery : Decoder ModeOfDelivery
decodeModeOfDelivery =
    string
        |> andThen
            (\mode ->
                case mode of
                    "svd-episiotomy" ->
                        succeed <| VaginalDelivery (Spontaneous True)

                    "svd-no-episiotomy" ->
                        succeed <| VaginalDelivery (Spontaneous False)

                    "vd-vacuum" ->
                        succeed <| VaginalDelivery WithVacuumExtraction

                    "cesarean-delivery" ->
                        succeed <| CesareanDelivery

                    _ ->
                        fail (mode ++ " is not a recognized ModeOfDelivery")
            )


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
                        fail <| String.fromInt ubudehe ++ " is out of range for Ubudehe"
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
                            String.fromInt educationLevel
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


decodeGeoField : Decoder String
decodeGeoField =
    string
        |> andThen (String.toLower >> toTitleCase >> succeed)
