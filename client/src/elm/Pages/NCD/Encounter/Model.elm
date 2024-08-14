module Pages.NCD.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showEndEncounterDialog : Bool
    }


type Msg
    = CloseEncounter NCDEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SetEndEncounterDialogState Bool


type Tab
    = Completed
    | Pending
    | Reports


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showEndEncounterDialog = False
    }
