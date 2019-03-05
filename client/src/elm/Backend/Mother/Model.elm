module Backend.Mother.Model exposing (ChildrenRelationType(..), EducationLevel(..), HIVStatus(..), MaritalStatus(..), Mother, educationLevelToString, hivStatusToString, stringToHivStatus)

import Backend.Patient.Model exposing (Gender, Ubudehe)
import Gizra.NominalDate exposing (NominalDate)


type ChildrenRelationType
    = MotherRelation
    | CaregiverRelation


type alias Mother =
    { name : String
    , firstName : String
    , middleName : Maybe String
    , secondName : String
    , nationalIdNumber : Maybe String
    , avatarUrl : Maybe String
    , birthDate : Maybe NominalDate
    , isDateOfBirthEstimated : Bool
    , relation : ChildrenRelationType
    , gender : Gender
    , ubudehe : Maybe Ubudehe
    , educationLevel : Maybe EducationLevel
    , profession : Maybe String
    , maritalStatus : Maybe MaritalStatus
    , hivStatus : Maybe HIVStatus
    , householdSize : Maybe Int
    , numberOfChildren : Maybe Int
    , province : Maybe String
    , district : Maybe String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
    , telephoneNumber : Maybe String
    , healthCenterName : Maybe String
    }


type EducationLevel
    = NoSchooling
    | PrimarySchool
    | VocationalTrainingSchool
    | SecondarySchool
    | DiplomaProgram
    | HigherEducation
    | AdvancedDiploma


type MaritalStatus
    = Divorced
    | Married
    | Single
    | Widowed


type HIVStatus
    = HIVExposedInfant
    | Negative
    | NegativeDiscordantCouple
    | Positive
    | Unknown


educationLevelToString : EducationLevel -> String
educationLevelToString educationLevel =
    case educationLevel of
        NoSchooling ->
            "0"

        PrimarySchool ->
            "1"

        VocationalTrainingSchool ->
            "2"

        SecondarySchool ->
            "3"

        DiplomaProgram ->
            "4"

        HigherEducation ->
            "5"

        AdvancedDiploma ->
            "6"


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


stringToHivStatus : String -> Maybe HIVStatus
stringToHivStatus string =
    case string of
        "hiv-exposed-infant" ->
            Just HIVExposedInfant

        "negative" ->
            Just Negative

        "negative-dc" ->
            Just NegativeDiscordantCouple

        "positive" ->
            Just Positive

        "unknown" ->
            Just Unknown

        _ ->
            Nothing
