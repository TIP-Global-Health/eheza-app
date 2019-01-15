module Backend.Patient.Decoder exposing (decodeGender, decodeUbudehe)

import Backend.Patient.Model exposing (Gender(..), Ubudehe(..))
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)


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
