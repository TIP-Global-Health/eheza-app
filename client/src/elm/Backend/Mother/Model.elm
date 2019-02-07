module Backend.Mother.Model exposing (ChildrenRelationType(..), EducationLevel(..), Mother, Ubudehe(..))

import Backend.Entities exposing (..)
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


type EducationLevel
    = NoSchooling
    | PrimarySchool
    | VocationalTrainingSchool
    | SecondarySchool
    | DiplomaProgram
    | HigherEducation
    | AdvancedDiploma
