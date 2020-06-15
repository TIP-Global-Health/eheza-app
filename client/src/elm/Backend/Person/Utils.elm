module Backend.Person.Utils exposing (ageInYears, diffInYears, expectedAgeByPerson, isAdult, isPersonAFertileWoman, isPersonAnAdult, initiatorFromUrlFragmemt, initiatorToUrlFragmemt, resolveExpectedAge)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Person.Model exposing (ExpectedAge(..), Gender(..), ParticipantDirectoryOperation(..), Person, Initiator(..))
import Date
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


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


expectedAgeByPerson : NominalDate -> Person -> ParticipantDirectoryOperation -> ExpectedAge
expectedAgeByPerson currentDate person operation =
    resolveExpectedAge currentDate person.birthDate operation


resolveExpectedAge : NominalDate -> Maybe NominalDate -> ParticipantDirectoryOperation -> ExpectedAge
resolveExpectedAge currentDate birthDate operation =
    case isAdult currentDate birthDate of
        Just True ->
            case operation of
                CreatePerson maybeId ->
                    -- Creating person with relation to adult => should be a child.
                    if isJust maybeId then
                        ExpectChild

                    else
                        -- Creating with no relation => should be a adult.
                        ExpectAdult

                EditPerson _ ->
                    ExpectAdult

        Just False ->
            case operation of
                CreatePerson maybeId ->
                    -- Creating person with relation to child => should be a adult.
                    if isJust maybeId then
                        ExpectAdult

                    else
                        -- Creating with no relation => should be a child.
                        ExpectChild

                EditPerson _ ->
                    ExpectChild

        Nothing ->
            ExpectAdultOrChild


initiatorToUrlFragmemt : Initiator -> String
initiatorToUrlFragmemt initiator =
    case initiator of
        ParticipantDirectoryOrigin ->
            "directory"

        IndividualEncounterOrigin encounterType ->
            case encounterType of
                AntenatalEncounter ->
                    "antenatal"

                InmmunizationEncounter ->
                    "inmmunization"

                NutritionEncounter ->
                    "nutrition"

        GroupEncounterOrigin sessionId ->
            "session-" ++ fromEntityUuid sessionId


initiatorFromUrlFragmemt : String -> Maybe Initiator
initiatorFromUrlFragmemt s =
    case s of
        "directory" ->
            Just ParticipantDirectoryOrigin

        "antenatal" ->
            IndividualEncounterOrigin AntenatalEncounter |> Just

        "inmmunization" ->
            IndividualEncounterOrigin InmmunizationEncounter |> Just

        "nutrition" ->
            IndividualEncounterOrigin NutritionEncounter |> Just

        _ ->
            let
                split =
                    String.split "-" s
            in
            case List.head split of
                Just "session" ->
                    if List.length split > 1 then
                        -- Remove the "session-" prefix.
                        String.dropLeft 8 s
                            |> toEntityUuid
                            |> GroupEncounterOrigin
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
