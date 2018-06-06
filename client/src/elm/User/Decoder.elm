module User.Decoder exposing (decodeUser)

import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (nullable, string, list, map, andThen, succeed, fail, Decoder)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Restful.Endpoint exposing (decodeEntityId)
import EverySet
import User.Model exposing (..)


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "id" decodeInt
        |> required "label" string
        |> optional "avatar_url" string "https://github.com/foo.png?s=90"
        |> optional "clinics" (list decodeEntityId) []
        |> optional "roles" (map EverySet.fromList (list decodeRole)) EverySet.empty


decodeRole : Decoder Role
decodeRole =
    string
        |> andThen
            (\s ->
                case s of
                    "anonymous user" ->
                        succeed Anonymous

                    "authenticated user" ->
                        succeed Authenticated

                    "administrator" ->
                        succeed Administrator

                    "nurse" ->
                        succeed Nurse

                    _ ->
                        fail <| s ++ " is not a recognized role"
            )
