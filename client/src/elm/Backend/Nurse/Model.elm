module Backend.Nurse.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender)
import Backend.Person.Model exposing (EducationLevel, MaritalStatus, Ubudehe)
import Backend.ResilienceMessage.Model exposing (ReasonForNotConsenting, ResilienceCategory, ResilienceMessage)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Time


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
    , resilienceNextReminder : Maybe Time.Posix
    , resilienceMessages : Dict ResilienceMessageId ResilienceMessage
    , resilienceConsentGiven : Bool
    , resilienceConsentReason : Maybe ReasonForNotConsenting
    }


type Role
    = RoleAdministrator
    | RoleCHW
    | RoleNurse
    | RoleLabTech


type ResilienceRole
    = ResilienceRoleCHW
    | ResilienceRoleNurse
    | ResilienceRoleLineManager
    | ResilienceRoleSupervisor
    | ResilienceRoleDirector


type alias Model =
    { updateNurse : WebData () }


emptyModel : Model
emptyModel =
    { updateNurse = NotAsked }


type Msg
    = UpdateNurse NurseId Nurse
    | HandleUpdatedNurse (WebData ())
