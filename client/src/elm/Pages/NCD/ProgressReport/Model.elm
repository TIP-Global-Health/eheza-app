module Pages.NCD.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { showEndEncounterDialog = False
    }


type Msg
    = NoOp
    | CloseEncounter NCDEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
