module Pages.ChildScoreboard.Encounter.Model exposing (..)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, NCDASign)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (NCDAData, NCDAForm, NCDAStep, emptyNCDAData)
import Pages.Page exposing (Page)


type alias Model =
    { ncdaData : NCDAData }


emptyModel : Model
emptyModel =
    { ncdaData = emptyNCDAData }


type alias AssembledData =
    { id : ChildScoreboardEncounterId
    , encounter : ChildScoreboardEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : ChildScoreboardMeasurements
    }


type Msg
    = CloseEncounter ChildScoreboardEncounterId
    | SetActivePage Page
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetNCDAFormStep NCDAStep
    | SetNCDAHelperState (Maybe NCDASign)
