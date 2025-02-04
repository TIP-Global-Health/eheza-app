module Backend.Person.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (individualEncounterTypeToString)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (..)
import Date
import Gizra.NominalDate exposing (NominalDate, diffMonths, diffYears, formatYYYYMMDD)
import Maybe.Extra exposing (isJust)
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


generateFullName : String -> String -> String
generateFullName first second =
    String.join " " [ second, first ]
        |> String.trim


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


isNewborn : NominalDate -> Person -> Maybe Bool
isNewborn currentDate person =
    ageInMonths currentDate person
        |> Maybe.map (\ageMonths -> ageMonths < 2)


isPersonAnAdult : NominalDate -> Person -> Maybe Bool
isPersonAnAdult currentDate person =
    isAdult currentDate person.birthDate


isPersonAFertileWoman : NominalDate -> Person -> Bool
isPersonAFertileWoman currentDate person =
    if person.gender == Male then
        False

    else
        person.birthDate
            |> Maybe.map (\birthDate -> diffYears birthDate currentDate |> (\age -> age > 13 && age < 50))
            |> Maybe.withDefault False


isChildUnderAgeOf2 : NominalDate -> Person -> Bool
isChildUnderAgeOf2 =
    isChildUnderAgeOf 2


isChildUnderAgeOf5 : NominalDate -> Person -> Bool
isChildUnderAgeOf5 =
    isChildUnderAgeOf 5


isChildUnderAgeOf : Int -> NominalDate -> Person -> Bool
isChildUnderAgeOf years currentDate person =
    ageInYears currentDate person
        |> Maybe.map (\age -> age < years)
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
                        -- Creating with no relation => should be an adult.
                        ExpectAdult

                EditPerson _ ->
                    ExpectAdult

        Just False ->
            case operation of
                CreatePerson maybeId ->
                    -- Creating person with relation to child => should be an adult.
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


initiatorToUrlFragment : Initiator -> String
initiatorToUrlFragment initiator =
    case initiator of
        ParticipantDirectoryOrigin ->
            "directory"

        IndividualEncounterOrigin encounterType ->
            individualEncounterTypeToString encounterType

        GroupEncounterOrigin sessionId ->
            "session-" ++ fromEntityUuid sessionId

        PrenatalNextStepsNewbornEnrolmentOrigin birthDate encounterId ->
            "prenatal-next-steps-" ++ formatYYYYMMDD birthDate ++ "-" ++ fromEntityUuid encounterId

        AcuteIllnessContactsTracingActivityOrigin _ ->
            -- Not in use, as at Acute Ilness patient is created
            -- from a dedicated form.
            ""


initiatorFromUrlFragment : String -> Maybe Initiator
initiatorFromUrlFragment s =
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

        "home-visit" ->
            IndividualEncounterOrigin WellChildEncounter |> Just

        "well-child" ->
            IndividualEncounterOrigin WellChildEncounter |> Just

        "ncd" ->
            IndividualEncounterOrigin NCDEncounter |> Just

        "child-scoreboard" ->
            IndividualEncounterOrigin ChildScoreboardEncounter |> Just

        "tuberculosis" ->
            IndividualEncounterOrigin TuberculosisEncounter |> Just

        "hiv" ->
            IndividualEncounterOrigin HIVEncounter |> Just

        _ ->
            if String.startsWith "session-" s then
                String.dropLeft 8 s
                    |> toEntityUuid
                    |> GroupEncounterOrigin
                    |> Just

            else if String.startsWith "prenatal-next-steps-" s then
                let
                    -- Format is YYYY-MM-DD-[UUID].
                    birthDateWithUuid =
                        String.dropLeft 20 s

                    birthDate =
                        String.left 10 birthDateWithUuid

                    uuid =
                        String.dropLeft 11 birthDateWithUuid
                in
                case String.split "-" birthDate of
                    [ yyyy, mm, dd ] ->
                        Maybe.map3
                            (\year month day ->
                                Just <|
                                    PrenatalNextStepsNewbornEnrolmentOrigin
                                        (Date.fromCalendarDate year (Date.numberToMonth month) day)
                                        (toEntityUuid uuid)
                            )
                            (String.toInt yyyy)
                            (String.toInt mm)
                            (String.toInt dd)
                            |> Maybe.withDefault Nothing

                    _ ->
                        Nothing

            else
                Nothing


graduatingAgeInMonth : Int
graduatingAgeInMonth =
    26


getHealthCenterName : Maybe HealthCenterId -> ModelIndexedDb -> Maybe String
getHealthCenterName healthCenterId db =
    Maybe.map2
        (\healthCenterId_ healthCenters ->
            Dict.get healthCenterId_ healthCenters
                |> Maybe.map .name
        )
        healthCenterId
        (RemoteData.toMaybe db.healthCenters)
        |> Maybe.Extra.join


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "male"

        Female ->
            "female"


genderFromString : String -> Maybe Gender
genderFromString s =
    case s of
        "female" ->
            Just Female

        "male" ->
            Just Male

        _ ->
            Nothing


modeOfDeliveryToString : ModeOfDelivery -> String
modeOfDeliveryToString mode =
    case mode of
        VaginalDelivery vaginal ->
            case vaginal of
                Spontaneous True ->
                    "svd-episiotomy"

                Spontaneous False ->
                    "svd-no-episiotomy"

                WithVacuumExtraction ->
                    "vd-vacuum"

        CesareanDelivery ->
            "cesarean-delivery"


hivStatusToString : HIVStatus -> String
hivStatusToString status =
    case status of
        HIVExposedInfant ->
            "hiv-exposed-infant"

        Negative ->
            "negative"

        NegativeDiscordantCouple ->
            "negative-dc"

        Positive ->
            "positive"

        Unknown ->
            "unknown"


ubudeheToInt : Ubudehe -> Int
ubudeheToInt ubudehe =
    case ubudehe of
        Ubudehe1 ->
            1

        Ubudehe2 ->
            2

        Ubudehe3 ->
            3

        Ubudehe4 ->
            4

        NoUbudehe ->
            0


ubudeheFromInt : Int -> Maybe Ubudehe
ubudeheFromInt value =
    case value of
        1 ->
            Just Ubudehe1

        2 ->
            Just Ubudehe2

        3 ->
            Just Ubudehe3

        4 ->
            Just Ubudehe4

        0 ->
            Just NoUbudehe

        _ ->
            Nothing


educationLevelToInt : EducationLevel -> Int
educationLevelToInt educationLevel =
    case educationLevel of
        NoSchooling ->
            0

        PrimarySchool ->
            1

        VocationalTrainingSchool ->
            2

        SecondarySchool ->
            3

        DiplomaProgram ->
            4

        HigherEducation ->
            5

        AdvancedDiploma ->
            6

        MastersDegree ->
            7


educationLevelFromInt : Int -> Maybe EducationLevel
educationLevelFromInt value =
    case value of
        0 ->
            Just NoSchooling

        1 ->
            Just PrimarySchool

        2 ->
            Just VocationalTrainingSchool

        3 ->
            Just SecondarySchool

        4 ->
            Just DiplomaProgram

        5 ->
            Just HigherEducation

        6 ->
            Just AdvancedDiploma

        7 ->
            Just MastersDegree

        _ ->
            Nothing


maritalStatusToString : MaritalStatus -> String
maritalStatusToString status =
    case status of
        Divorced ->
            "divorced"

        Married ->
            "married"

        Single ->
            "single"

        Widowed ->
            "widowed"

        LivingWithPartner ->
            "living-with-partner"

        Religious ->
            "religious"


maritalStatusFromString : String -> Maybe MaritalStatus
maritalStatusFromString value =
    case value of
        "divorced" ->
            Just Divorced

        "married" ->
            Just Married

        "single" ->
            Just Single

        "widowed" ->
            Just Widowed

        "living-with-partner" ->
            Just LivingWithPartner

        "religious" ->
            Just Religious

        _ ->
            Nothing
