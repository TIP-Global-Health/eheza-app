module Backend.Nurse.Decoder exposing (decodeNurse)

import AssocList as Dict exposing (Dict)
import Backend.Nurse.Model exposing (..)
import Backend.Nurse.Utils exposing (resilienceRoleFromString)
import Backend.Person.Decoder
    exposing
        ( decodeEducationLevel
        , decodeGender
        , decodeMaritalStatus
        , decodeUbudehe
        )
import Backend.ResilienceMessage.Decoder exposing (decodeResilienceMessages)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Gizra.TimePosix exposing (decodeSecondsAsPosix)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeNurse : Decoder Nurse
decodeNurse =
    succeed Nurse
        |> required "label" string
        |> optional "health_centers" (map EverySet.fromList (list decodeEntityUuid)) EverySet.empty
        |> optional "villages" (map EverySet.fromList (list decodeEntityUuid)) EverySet.empty
        |> optional "role" decodeRoles EverySet.empty
        |> optional "email" (map Just string) Nothing
        |> required "pin_code" string
        |> optional "resilience_program" bool False
        |> optional "resilience_start_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "resilience_role" (nullable decodeResilienceRole) Nothing
        |> optional "birth_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "gender" (nullable decodeGender) Nothing
        |> optional "education_level" (nullable decodeEducationLevel) Nothing
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "marital_status" (nullable decodeMaritalStatus) Nothing
        |> optional "next_reminder" (nullable decodeSecondsAsPosix) Nothing
        |> optional "resilience_messages" decodeResilienceMessages Dict.empty
        |> optional "resilience_consent_given" (nullable bool) Nothing


decodeRoles : Decoder (EverySet Role)
decodeRoles =
    oneOf
        [ map EverySet.fromList (list decodeRole)
        , succeed (EverySet.singleton RoleNurse)
        ]


decodeRole : Decoder Role
decodeRole =
    string
        |> andThen
            (\s ->
                case s of
                    "admin" ->
                        succeed RoleAdministrator

                    "nurse" ->
                        succeed RoleNurse

                    "chw" ->
                        succeed RoleCHW

                    "lab-tech" ->
                        succeed RoleLabTech

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized Role."
            )


decodeResilienceRole : Decoder ResilienceRole
decodeResilienceRole =
    string
        |> andThen
            (\s ->
                resilienceRoleFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault
                        (fail <|
                            s
                                ++ " is not a recognized ResilienceRole."
                        )
            )
