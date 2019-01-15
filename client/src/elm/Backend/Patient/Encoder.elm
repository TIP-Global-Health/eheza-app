module Backend.Patient.Encoder exposing (encodeGender, encodeUbudehe)

import Backend.Patient.Model exposing (Gender(..), Ubudehe(..))
import Json.Encode exposing (..)


encodeGender : Gender -> Value
encodeGender gender =
    case gender of
        Male ->
            string "male"

        Female ->
            string "female"


encodeUbudehe : Ubudehe -> Value
encodeUbudehe ubudehe =
    case ubudehe of
        Ubudehe1 ->
            int 1

        Ubudehe2 ->
            int 2

        Ubudehe3 ->
            int 3

        Ubudehe4 ->
            int 4
