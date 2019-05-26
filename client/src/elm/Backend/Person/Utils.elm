module Backend.Person.Utils exposing (ageInYears, diffInYears)

import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Time.Date


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (Time.Date.delta currentDate >> .years) comparedDate
