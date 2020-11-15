module Pages.AcuteIllnessOutcome.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome(..))
import Date exposing (Date)
import Pages.Page exposing (Page)


type alias Model =
    { acuteIllnessOutcome : Maybe AcuteIllnessOutcome
    , showAlertsDialog : Bool
    }


emptyModel : Model
emptyModel =
    { acuteIllnessOutcome = Nothing
    , showAlertsDialog = False
    }


type Msg
    = NoOp
    | SaveAcuteIllnessOutcome
    | SetActivePage Page
    | SetAcuteIllnessOutcome String
    | SetAlertsDialogState Bool
