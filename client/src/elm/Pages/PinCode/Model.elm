module Pages.PinCode.Model exposing (DisplayMenu(..), Model, Msg(..), OutMsg(..), emptyModel)

{-| This models the PinCode entered by the user.
-}

import Backend.Entities exposing (HealthCenterId)
import Pages.Page exposing (Page)


type alias Model =
    { code : String
    , menu : DisplayMenu
    }


type Msg
    = ClearPinCode
    | HandleLoginClicked
    | HandleLogoutClicked
    | SetDisplayMenu DisplayMenu
    | SendOutMsg OutMsg
    | SetPinCode String


{-| The message we return when we want to actually attempt a login, or logout.
Whoever calls `update` needs to detect this and do the correct thing.
-}
type OutMsg
    = Logout
    | TryPinCode String
    | SetActivePage Page
    | GoToRandomPrenatalEncounter
    | SetHealthCenter HealthCenterId


type DisplayMenu
    = ClinicalMenu
    | MainMenu


emptyModel : Model
emptyModel =
    { code = ""
    , menu = MainMenu
    }
