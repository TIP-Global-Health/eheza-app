module Pages.WellChild.Encounter.Model exposing (AssembledData, DialogType(..), ECDPopupType(..), Model, Msg(..), Tab(..), WarningPopupType(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (WellChildMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity)
import Backend.WellChildEncounter.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (VaccinationProgressDict)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , skippedActivities : EverySet WellChildActivity
    , dialogState : Maybe DialogType
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , skippedActivities = EverySet.empty
    , dialogState = Nothing
    }


type Msg
    = CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | TriggerAcuteIllnessEncounter PersonId WellChildEncounterId
    | NavigateToActivity WellChildEncounterId WellChildActivity
    | SkipActivity WellChildActivity
    | SetDialogState (Maybe DialogType)


type DialogType
    = DialogWarning WarningPopupType
    | DialogSkipNCDA


type WarningPopupType
    = PopupDangerSigns
    | PopupECD ECDPopupType


type ECDPopupType
    = ChildBehind
    | ReferToSpecialist


type Tab
    = Completed
    | Pending
    | Reports


type alias AssembledData =
    { id : WellChildEncounterId
    , encounter : WellChildEncounter
    , participant : IndividualEncounterParticipant
    , person : Person
    , measurements : WellChildMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    , vaccinationHistory : VaccinationProgressDict

    -- Similar to vaccinationHistory, but includes immunisation data of current encounter.
    , vaccinationProgress : VaccinationProgressDict
    }
