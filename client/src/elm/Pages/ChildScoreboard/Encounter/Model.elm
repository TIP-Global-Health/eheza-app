module Pages.ChildScoreboard.Encounter.Model exposing (..)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, NCDASign, NCDASignNEW, NutritionSupplementType)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (NCDADataNEW, NCDAFormNEW, NCDAStepNEW, emptyNCDADataNEW)
import Pages.Page exposing (Page)


type alias Model =
    { ncdaData : NCDADataNEW
    , showAIEncounterPopup : Bool
    }


emptyModel : Model
emptyModel =
    { ncdaData = emptyNCDADataNEW
    , showAIEncounterPopup = False
    }


type alias AssembledData =
    { id : ChildScoreboardEncounterId
    , encounter : ChildScoreboardEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : ChildScoreboardMeasurements
    , ancEncounters : Maybe Int
    }


type Msg
    = CloseEncounter AssembledData
    | SetActivePage Page
    | SetNCDABoolInput (Bool -> NCDAFormNEW -> NCDAFormNEW) Bool
    | SetBirthWeight String
    | SetNumberANCVisits String
    | SetNutritionSupplementType NutritionSupplementType
    | SetNCDAFormStep NCDAStepNEW
    | SetNCDAHelperState (Maybe NCDASignNEW)
    | ShowAIEncounterPopup
    | TriggerAcuteIllnessEncounter AssembledData
