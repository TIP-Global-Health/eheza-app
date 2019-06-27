module Backend.Person.Model exposing (EducationLevel(..), Gender(..), HIVStatus(..), MaritalStatus(..), ModeOfDelivery(..), Person, Ubudehe(..), VaginalDelivery(..), allEducationLevels, allHivStatuses, allMaritalStatuses, allModesOfDelivery, allUbudehes)

import Backend.Entities exposing (HealthCenterId)
import Gizra.NominalDate exposing (NominalDate)


type alias Person =
    { name : String
    , firstName : String
    , secondName : String
    , nationalIdNumber : Maybe String
    , childBirthOrder : Maybe String
    , avatarUrl : Maybe String
    , birthDate : Maybe NominalDate
    , isDateOfBirthEstimated : Bool
    , gender : Gender
    , hivStatus : Maybe HIVStatus
    , numberOfChildren : Maybe Int
    , modeOfDelivery : Maybe ModeOfDelivery
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


type ModeOfDelivery
    = VaginalDelivery VaginalDelivery
    | CesareanDelivery


{-| The bool indicates whether an episiotomy was performed
-}
type VaginalDelivery
    = Spontaneous Bool
    | WithVacuumExtraction


allModesOfDelivery : List ModeOfDelivery
allModesOfDelivery =
    [ VaginalDelivery (Spontaneous True)
    , VaginalDelivery (Spontaneous False)
    , VaginalDelivery WithVacuumExtraction
    , CesareanDelivery
    ]


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
