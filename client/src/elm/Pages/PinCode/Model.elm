module Pages.PinCode.Model exposing (..)

{-| This models the PinCode entered by the user.
-}

import Backend.Entities exposing (HealthCenterId, NurseId, VillageId)
import Backend.Nurse.Model exposing (Nurse)
import Pages.Page exposing (Page)


type alias Model =
    { code : String
    , notifyOfUnreadMessages : Bool
    }


emptyModel : Model
emptyModel =
    { code = ""
    , notifyOfUnreadMessages = False
    }


type Msg
    = ClearPinCode
    | HandleLoginClicked
    | HandleLogoutClicked
    | SendOutMsg OutMsg
    | SetPinCode String
    | SetNotifyOfUnreadMessages Bool


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
    | MenuMessagingCenter


type ResilienceReminderType
    = ResilienceReminderDrinkWatter
    | ResilienceReminderTakeBreak
