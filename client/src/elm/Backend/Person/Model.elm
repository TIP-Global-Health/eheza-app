module Backend.Person.Model exposing (EducationLevel(..), Gender(..), MaritalStatus(..), Person, Ubudehe(..), allEducationLevels, allMaritalStatuses, allUbudehes)

import Backend.Entities exposing (HealthCenterId)
import Gizra.NominalDate exposing (NominalDate)


type alias Person =
    { name : String
    , firstName : String
    , secondName : String
    , nationalIdNumber : Maybe String
    , avatarUrl : Maybe String
    , birthDate : Maybe NominalDate
    , isDateOfBirthEstimated : Bool
    , gender : Gender
    , ubudehe : Maybe Ubudehe
    , educationLevel : Maybe EducationLevel
    , maritalStatus : Maybe MaritalStatus
    , province : Maybe String
    , district : Maybe String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
    , telephoneNumber : Maybe String
    , healthCenterId : Maybe HealthCenterId
    }


type Gender
    = Female
    | Male


type Ubudehe
    = Ubudehe1
    | Ubudehe2
    | Ubudehe3
    | Ubudehe4


allUbudehes : List Ubudehe
allUbudehes =
    [ Ubudehe1
    , Ubudehe2
    , Ubudehe3
    , Ubudehe4
    ]


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
