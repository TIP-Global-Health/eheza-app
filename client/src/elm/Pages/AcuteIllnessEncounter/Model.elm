module Pages.AcuteIllnessEncounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

-- import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    , showEndEncounetrDialog : Bool
    }


type Msg
    = CloseEncounter AcuteIllnessEncounterId
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetEndEncounterDialogState Bool
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showEndEncounetrDialog = False
    }
