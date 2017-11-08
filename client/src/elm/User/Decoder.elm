module User.Decoder exposing (decodeUser)

import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (nullable, string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Restful.Endpoint exposing (decodeEntityId)
import User.Model exposing (..)


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "id" decodeInt
        |> required "label" string
        |> optional "avatar_url" string "https://github.com/foo.png?s=90"
        |> optional "clinics" (list decodeEntityId) []
