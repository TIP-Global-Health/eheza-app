module Backend.Person.Utils exposing (ageInYears, diffInYears, isAdult, isPersonAddressSet, isPersonAnAdult)

import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Maybe.Extra exposing (isJust)
import Time.Date


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (Time.Date.delta currentDate >> .years) comparedDate


isAdult : NominalDate -> Maybe NominalDate -> Maybe Bool
isAdult currentDate maybeBirthDate =
    maybeBirthDate
        |> diffInYears currentDate
        |> Maybe.map ((<) 12)


isPersonAnAdult : NominalDate -> Person -> Maybe Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate


isPersonAddressSet : Person -> Bool
isPersonAddressSet person =
    [ person.province, person.district, person.sector, person.cell, person.village ]
        |> List.all isJust
