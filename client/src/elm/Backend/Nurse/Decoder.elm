module Backend.Nurse.Decoder exposing (decodeNurse)

import Backend.Nurse.Model exposing (..)
import EverySet
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeNurse : Decoder Nurse
decodeNurse =
    decode Nurse
        |> required "label" string
        |> required "pin_code" string
        |> required "clinics" (list decodeEntityUuid)
        |> required "role" (map EverySet.fromList (list decodeRole))
        |> optional "email" (map Just string) Nothing


decodeRole : Decoder Role
decodeRole =
    string
        |> andThen
            (\s ->
                case s of
                    "nurse" ->
                        succeed RoleNurse

                    "administrator" ->
                        succeed RoleAdministrator

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized role."
            )
