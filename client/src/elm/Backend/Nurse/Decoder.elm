module Backend.Nurse.Decoder exposing (decodeNurse)

import Backend.Nurse.Model exposing (..)
import EverySet exposing (EverySet)
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
                    "nurse" ->
                        succeed RoleNurse

                    "admin" ->
                        succeed RoleAdministrator

                    "chw" ->
                        succeed RoleCHW

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized role."
            )
