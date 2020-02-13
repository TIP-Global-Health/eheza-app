module Backend.Person.Utils exposing (ageInYears, diffInYears, isAdult, isPersonAFertileWoman, isPersonAnAdult)

import Backend.Person.Model exposing (Gender(..), Person)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
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


isPersonAFertileWoman : NominalDate -> Person -> Bool
isPersonAFertileWoman currentDate person =
    if person.gender == Male then
        False

    else
        person.birthDate
            |> diffInYears currentDate
            |> Maybe.map
                (\age -> age > 12 && age < 45)
            |> Maybe.withDefault False
