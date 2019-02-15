module Backend.Mother.Model exposing (EducationLevel(..), Mother, Ubudehe(..))

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias Mother =
    { name : String
    , avatarUrl : Maybe String
    , children : List ChildId
    , birthDate : Maybe NominalDate
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
