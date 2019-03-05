module Backend.Mother.Model exposing (ChildrenRelationType(..), EducationLevel(..), HIVStatus(..), MaritalStatus(..), Mother, Ubudehe(..), allEducationLevels, allHivStatuses, allMaritalStatuses, allUbudehes)

import Gizra.NominalDate exposing (NominalDate)


type ChildrenRelationType
    = MotherRelation
    | CaregiverRelation


type alias Mother =
    { name : String
    , avatarUrl : Maybe String
    , birthDate : Maybe NominalDate
    , relation : ChildrenRelationType
    , ubudehe : Maybe Ubudehe
    , educationLevel : Maybe EducationLevel
    }


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
