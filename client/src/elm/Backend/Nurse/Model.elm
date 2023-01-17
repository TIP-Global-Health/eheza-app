module Backend.Nurse.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender)
import Backend.Person.Model exposing (EducationLevel(..), MaritalStatus(..), Ubudehe(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)


type alias Nurse =
    { name : String
    , healthCenters : EverySet HealthCenterId
    , villages : EverySet VillageId
    , roles : EverySet Role
    , email : Maybe String
    , pinCode : String
    , resilienceProgramEnabled : Bool
    , resilienceProgramStartDate : Maybe NominalDate
    , resilienceRole : Maybe ResilienceRole
    , resilienceBirthDate : Maybe NominalDate
    , resilienceGender : Maybe Gender
    , resilienceEducationLevel : Maybe EducationLevel
    , resilienceUbudehe : Maybe Ubudehe
    , resilienceMaritalStatus : Maybe MaritalStatus
    }


type Role
    = RoleAdministrator
    | RoleCHW
    | RoleNurse


type ResilienceRole
    = ResilienceRoleCHW
    | ResilienceRoleNurse
    | ResilienceRoleLineManager
    | ResilienceRoleSupervisor
    | ResilienceRoleDirector


allEducationLevels : List EducationLevel
allEducationLevels =
    [ NoSchooling
    , PrimarySchool
    , VocationalTrainingSchool
    , SecondarySchool
    , AdvancedDiploma
    , HigherEducation
    , MastersDegree
    ]


allMaritalStatuses : List MaritalStatus
allMaritalStatuses =
    [ Divorced
    , Married
    , Single
    , Widowed
    , LivingWithPartner
    , Religious
    ]
