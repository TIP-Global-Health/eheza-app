module Backend.Person.Utils exposing (ageInYears)

import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Time.Date


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    Maybe.map (Time.Date.delta currentDate >> .years) person.birthDate
