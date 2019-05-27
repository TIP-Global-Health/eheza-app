module Backend.Person.Utils exposing (ageInYears, diffInYears, isMotherRegistering)

import Backend.Person.Model exposing (Person)
import Date
import Form exposing (FieldState)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Result
import Time.Date


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (Time.Date.delta currentDate >> .years) comparedDate


isMotherRegistering : NominalDate -> FieldState e String -> Bool
isMotherRegistering currentDate birthDateField =
    birthDateField.value
        |> Maybe.andThen (Date.fromString >> Result.toMaybe)
        |> Maybe.map fromLocalDateTime
        |> diffInYears currentDate
        |> Maybe.map ((<) 12)
        |> Maybe.withDefault False
