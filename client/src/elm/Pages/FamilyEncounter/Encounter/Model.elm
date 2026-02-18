module Pages.FamilyEncounter.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.FamilyActivity.Model exposing (FamilyActivity)
import Backend.FamilyEncounter.Model exposing (FamilyEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (FamilyMeasurements)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , skippedActivities : EverySet FamilyActivity
    , dialogState : Maybe DialogType
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , skippedActivities = EverySet.empty
    , dialogState = Nothing
    }


type Msg
    = CloseEncounter FamilyEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SkipActivity FamilyActivity
    | SetDialogState (Maybe DialogType)


type Tab
    = Completed
    | Pending
    | Reports


type DialogType
    = DialogEndEncounter


type alias AssembledData =
    { id : FamilyEncounterId
    , encounter : FamilyEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : FamilyMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( FamilyEncounterId, FamilyMeasurements ) )
    }
