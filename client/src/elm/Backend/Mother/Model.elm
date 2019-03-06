module Backend.Mother.Model exposing (ChildrenRelationType(..), EducationLevel(..), HIVStatus(..), MaritalStatus(..), Mother, allEducationLevels, allHivStatuses, allMaritalStatuses)

import Backend.Entities exposing (..)
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
    , clinic : Maybe ClinicId

    -- TODO: Convert to HealthCenterId, and create on backend.
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


allEducationLevels : List EducationLevel
allEducationLevels =
    [ NoSchooling
    , PrimarySchool
    , VocationalTrainingSchool
    , SecondarySchool
    , DiplomaProgram
    , HigherEducation
    , AdvancedDiploma
    ]


type MaritalStatus
    = Divorced
    | Married
    | Single
    | Widowed


allMaritalStatuses : List MaritalStatus
allMaritalStatuses =
    [ Divorced
    , Married
    , Single
    , Widowed
    ]


type HIVStatus
    = HIVExposedInfant
    | Negative
    | NegativeDiscordantCouple
    | Positive
    | Unknown


allHivStatuses : List HIVStatus
allHivStatuses =
    [ HIVExposedInfant
    , Negative
    , NegativeDiscordantCouple
    , Positive
    , Unknown
    ]
