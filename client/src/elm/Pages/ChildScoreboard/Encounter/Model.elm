module Pages.ChildScoreboard.Encounter.Model exposing (..)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, NCDASign, NutritionSupplementType)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (NCDAData, NCDAForm, NCDAStep, emptyNCDAData)
import Pages.Page exposing (Page)


type alias Model =
    { ncdaData : NCDAData
    , showAIEncounterPopup : Bool
    }


emptyModel : Model
emptyModel =
    { ncdaData = emptyNCDAData
    , showAIEncounterPopup = False
    }


type alias AssembledData =
    { id : ChildScoreboardEncounterId
    , encounter : ChildScoreboardEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : ChildScoreboardMeasurements
    }


type Msg
    = CloseEncounter AssembledData
    | SetActivePage Page
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetNumberANCVisits String
    | SetNutritionSupplementType NutritionSupplementType
    | SetNCDAFormStep NCDAStep
    | SetNCDAHelperState (Maybe NCDASign)
    | ShowAIEncounterPopup
    | TriggerAcuteIllnessEncounter AssembledData