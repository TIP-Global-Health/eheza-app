module Backend.Person.Model exposing
    ( EducationLevel(..)
    , ExpectedAge(..)
    , ExpectedGender(..)
    , Gender(..)
    , HIVStatus(..)
    , MaritalStatus(..)
    , ModeOfDelivery(..)
    , ParticipantDirectoryOperation(..)
    , Person
    , Initiator(..)
    , Ubudehe(..)
    , VaginalDelivery(..)
    , allEducationLevels
    , allHivStatuses
    , allMaritalStatuses
    , allModesOfDelivery
    , allUbudehes
    )

import Backend.Entities exposing (HealthCenterId, PersonId, SessionId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Gizra.NominalDate exposing (NominalDate)


type alias Person =
    { name : String
    , firstName : String
    , secondName : String
    , nationalIdNumber : Maybe String
    , hmisNumber : Maybe String
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
    , shard : Maybe HealthCenterId
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


type Initiator
    = ParticipantDirectoryOrigin
    | IndividualEncounterOrigin IndividualEncounterType
    | GroupEncounterOrigin SessionId


type ParticipantDirectoryOperation
    = CreatePerson (Maybe PersonId)
    | EditPerson PersonId


{-| Sometimes, we are in a state where we are expecting the user to enter an
adult or a child, and sometimes we don't care -- they could be entering either.

This controls various aspects of validation. If set to `ExpectAdultOrChild`, we
also check the birth date actually entered in the form, and validate the rest
according that birth date.

-}
type ExpectedAge
    = ExpectAdult
    | ExpectChild
    | ExpectAdultOrChild


type ExpectedGender
    = ExpectMale
    | ExpectFemale
    | ExpectMaleOrFemale
