module Backend.Person.Utils exposing (ageInYears, diffInMonths, diffInYears, isAdult, isPersonAnAdult)

import Backend.Person.Model exposing (Person)
import Date
import Gizra.NominalDate exposing (NominalDate)


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (\compared -> Date.diff Date.Years compared currentDate) comparedDate


diffInMonths : NominalDate -> NominalDate -> Int
diffInMonths currentDate comparedDate =
    Date.diff Date.Months comparedDate currentDate


isAdult : NominalDate -> Maybe NominalDate -> Maybe Bool
isAdult currentDate maybeBirthDate =
    maybeBirthDate
        |> diffInYears currentDate
        |> Maybe.map ((<) 12)


isPersonAnAdult : NominalDate -> Person -> Maybe Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate
