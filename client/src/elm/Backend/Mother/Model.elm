module Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..), Mother, toStringEducationLevel)

import Backend.Entities exposing (..)
import Backend.Patient.Model exposing (Gender, Ubudehe)
import Gizra.NominalDate exposing (NominalDate)


type alias Mother =
    { name : String
    , firstName : String
    , middleName : Maybe String
    , secondName : String
    , nationalIdNumber : Maybe String
    , avatarUrl : Maybe String
    , children : List ChildId
    , birthDate : NominalDate
    , isDateOfBirthEstimated : Bool
    , gender : Gender
    , educationLevel : Maybe EducationLevel
    , profession : Maybe String
    , maritalStatus : Maybe MaritalStatus
    , hivStatus : Maybe HIVStatus
    , ubudehe : Maybe Ubudehe
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
    | Maried
    | Single
    | Widowed


type HIVStatus
    = Negative
    | NA
    | Positive


toStringEducationLevel : EducationLevel -> String
toStringEducationLevel educationLevel =
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
