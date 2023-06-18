module Pages.ChildScoreboard.Encounter.Model exposing (..)

import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, NCDASign, NCDASignNEW)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (NCDADataNEW, NCDAFormNEW, NCDAStepNEW, emptyNCDADataNEW)
import Pages.Page exposing (Page)


type alias Model =
    { ncdaData : NCDADataNEW }


emptyModel : Model
emptyModel =
    { ncdaData = emptyNCDADataNEW }


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
    | SetNCDABoolInput (Bool -> NCDAFormNEW -> NCDAFormNEW) Bool
    | SetBirthWeight String
    | SetNumberANCVisitsMsg String
    | SetNCDAFormStep NCDAStepNEW
    | SetNCDAHelperState (Maybe NCDASignNEW)
