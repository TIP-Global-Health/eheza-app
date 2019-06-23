module Backend.Person.Utils exposing (ageInYears, diffInYears, isAdult, isPersonAnAdult)

import Backend.Person.Model exposing (Person)
import Date
import Gizra.NominalDate exposing (NominalDate)


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (Date.diff Date.Years currentDate) comparedDate


isAdult : NominalDate -> Maybe NominalDate -> Maybe Bool
isAdult currentDate maybeBirthDate =
    maybeBirthDate
        |> diffInYears currentDate
        |> Maybe.map ((<) 12)


isPersonAnAdult : NominalDate -> Person -> Maybe Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate
