module Backend.Person.Utils exposing (ageInYears, diffInYears, expectedAgeByPerson, isAdult, isPersonAnAdult, resolveExpectedAge)

import Backend.Person.Model exposing (ExpectedAge(..), ParticipantDirectoryOperation(..), Person)
import Date
import Gizra.NominalDate exposing (NominalDate)


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    diffInYears currentDate person.birthDate


diffInYears : NominalDate -> Maybe NominalDate -> Maybe Int
diffInYears currentDate comparedDate =
    Maybe.map (\compared -> Date.diff Date.Years compared currentDate) comparedDate


isAdult : NominalDate -> Maybe NominalDate -> Maybe Bool
isAdult currentDate maybeBirthDate =
    maybeBirthDate
        |> diffInYears currentDate
        |> Maybe.map ((<) 12)


isPersonAnAdult : NominalDate -> Person -> Maybe Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate


expectedAgeByPerson : NominalDate -> Person -> ParticipantDirectoryOperation -> ExpectedAge
expectedAgeByPerson currentDate person operation =
    resolveExpectedAge currentDate person.birthDate operation


resolveExpectedAge : NominalDate -> Maybe NominalDate -> ParticipantDirectoryOperation -> ExpectedAge
resolveExpectedAge currentDate birthDate operation =
    case isAdult currentDate birthDate of
        Just True ->
            case operation of
                CreatePerson ->
                    ExpectChild

                EditPerson ->
                    ExpectAdult

        Just False ->
            case operation of
                CreatePerson ->
                    ExpectAdult

                EditPerson ->
                    ExpectChild

        Nothing ->
            ExpectAdultOrChild
