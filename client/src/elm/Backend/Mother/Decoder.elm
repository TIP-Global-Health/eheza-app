module Backend.Mother.Decoder exposing (decodeHivStatus, decodeMother)

import Backend.Mother.Model exposing (..)
import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (Gender(..))
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeMother : Decoder Mother
decodeMother =
    decode Mother
        |> required "label" string
        |> optional "first_name" string ""
        |> optional "middle_name" (nullable string) Nothing
        |> optional "second_name" string ""
        |> optional "national_id_number" (nullable string) Nothing
        |> optional "avatar" (nullable string) Nothing
        |> optional "date_birth" (nullable decodeYYYYMMDD) Nothing
        |> optional "birth_date_estimated" bool False
        |> optional "relation" decodeChildrenRelation MotherRelation
        |> optional "gender" decodeGender Female
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "education_level" (nullable decodeEducationLevel) Nothing
        |> optional "profession" (nullable string) Nothing
        |> optional "marital_status" (nullable decodeMaritalStatus) Nothing
        |> optional "hiv_status" (nullable decodeHivStatus) Nothing
        |> optional "household_size" (nullable decodeInt) Nothing
        |> optional "number_of_children" (nullable decodeInt) Nothing
        |> optional "province" (nullable string) Nothing
        |> optional "district" (nullable string) Nothing
        |> optional "sector" (nullable string) Nothing
        |> optional "cell" (nullable string) Nothing
        |> optional "village" (nullable string) Nothing
        |> optional "phone_number" (nullable string) Nothing
        |> optional "clinic" (nullable decodeEntityUuid) Nothing
        |> optional "health_center" (nullable decodeEntityUuid) Nothing


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
