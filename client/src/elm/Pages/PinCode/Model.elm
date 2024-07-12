module Pages.PinCode.Model exposing (..)

{-| This models the PinCode entered by the user.
-}

import Backend.Entities exposing (HealthCenterId, NurseId, VillageId)
import Backend.Nurse.Model exposing (Nurse)
import Pages.Page exposing (Page)
import Time


type alias Model =
    { code : String
    , nextNotification : Maybe Time.Posix
    }


emptyModel : Model
emptyModel =
    { code = ""
    , nextNotification = Nothing
    }


type Msg
    = HandleLoginClicked
    | HandleLogoutClicked
    | SendOutMsg OutMsg
    | SetPinCode String
    | SetNextNotification Time.Posix
    | HandleNotificationResponse Bool


{-| The message we return when we want to actually attempt a login, or logout.
Whoever calls `update` needs to detect this and do the correct thing.
-}
type OutMsg
    = Logout
    | TryPinCode String
    | SetActivePage Page
    | SetHealthCenter HealthCenterId
    | SetVillage VillageId
    | UpdateNurse NurseId Nurse


type MainMenuActivity
    = MenuClinical
    | MenuParticipantDirectory
    | MenuDashboards
    | MenuCaseManagement
    | MenuDeviceStatus
    | MenuWellbeing
    | MenuStockManagement


type ResilienceReminderType
    = ResilienceReminderDrinkWatter
    | ResilienceReminderTakeBreak
