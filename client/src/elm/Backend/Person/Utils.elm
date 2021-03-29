module Backend.Person.Utils exposing (..)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Person.Model exposing (ExpectedAge(..), Gender(..), Initiator(..), ParticipantDirectoryOperation(..), Person)
import Date
import Gizra.NominalDate exposing (NominalDate, diffMonths, diffYears)
import Maybe.Extra exposing (isJust)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


ageInYears : NominalDate -> Person -> Maybe Int
ageInYears currentDate person =
    person.birthDate
        |> Maybe.map (\birthDate -> diffYears birthDate currentDate)


ageInMonths : NominalDate -> Person -> Maybe Int
ageInMonths currentDate person =
    person.birthDate
        |> Maybe.map (\birthDate -> diffMonths birthDate currentDate)


isAdult : NominalDate -> Maybe NominalDate -> Maybe Bool
isAdult currentDate maybeBirthDate =
    maybeBirthDate
        |> Maybe.map (\birthDate -> diffYears birthDate currentDate |> (<) 12)


isPersonAnAdult : NominalDate -> Person -> Maybe Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate


isPersonAFertileWoman : NominalDate -> Person -> Bool
isPersonAFertileWoman currentDate person =
    if person.gender == Male then
        False

    else
        person.birthDate
            |> Maybe.map (\birthDate -> diffYears birthDate currentDate |> (\age -> age > 12 && age < 45))
            |> Maybe.withDefault False


isChildUnderAgeOf5 : NominalDate -> Person -> Bool
isChildUnderAgeOf5 currentDate person =
    ageInYears currentDate person
        |> Maybe.map (\age -> age < 5)
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


defaultIconForPerson : NominalDate -> Person -> String
defaultIconForPerson currentDate person =
    isPersonAnAdult currentDate person
        |> Maybe.map
            (\adult ->
                if adult then
                    "mother"

                else
                    "child"
            )
        |> Maybe.withDefault "mother"


initiatorToUrlFragmemt : Initiator -> String
initiatorToUrlFragmemt initiator =
    case initiator of
        ParticipantDirectoryOrigin ->
            "directory"

        IndividualEncounterOrigin encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    "acute-illness"

                AntenatalEncounter ->
                    "antenatal"

                HomeVisitEncounter ->
                    "home-visit"

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

        "acute-illness" ->
            IndividualEncounterOrigin AcuteIllnessEncounter |> Just

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


graduatingAgeInMonth : Int
graduatingAgeInMonth =
    26
