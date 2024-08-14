module Pages.AcuteIllness.Outcome.Model exposing (Model, Msg(..), emptyModel)

import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome)
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
    = SaveAcuteIllnessOutcome
    | SetActivePage Page
    | SetAcuteIllnessOutcome String
    | SetAlertsDialogState Bool
