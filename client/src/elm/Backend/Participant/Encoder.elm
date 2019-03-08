module Backend.Participant.Encoder exposing (encodeGender, encodeUbudehe)

import Backend.Participant.Model exposing (Gender(..), Ubudehe(..))
import Json.Encode exposing (..)


encodeGender : Gender -> Value
encodeGender gender =
    case gender of
        Male ->
            string "male"

        Female ->
            string "female"


encodeUbudehe : Ubudehe -> Int
encodeUbudehe ubudehe =
    case ubudehe of
        Ubudehe1 ->
            1

        Ubudehe2 ->
            2

        Ubudehe3 ->
            3

        Ubudehe4 ->
            4
