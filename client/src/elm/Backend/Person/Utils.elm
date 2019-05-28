module Backend.Person.Utils exposing (ageInYears, diffInYears, isAdultRegistering, isPersonAddressSet, isPersonAnAdult)

import Backend.Person.Model exposing (Person)
import Date
import Form exposing (FieldState)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Maybe.Extra exposing (isJust)
import Result
import Time.Date


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (Time.Date.delta currentDate >> .years) comparedDate


isAdultRegistering : NominalDate -> FieldState e String -> Bool
isAdultRegistering currentDate birthDateField =
    birthDateField.value
        |> Maybe.andThen (Date.fromString >> Result.toMaybe)
        |> Maybe.map fromLocalDateTime
        |> isAdult currentDate


isAdult : NominalDate -> Maybe NominalDate -> Bool
isAdult currentDate maybeBirthDate =
    maybeBirthDate
        |> diffInYears currentDate
        |> Maybe.map ((<) 12)
        |> Maybe.withDefault True


isPersonAnAdult : NominalDate -> Person -> Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate


isPersonAddressSet : Person -> Bool
isPersonAddressSet person =
    [ person.province, person.district, person.sector, person.cell, person.village ]
        |> List.all isJust
