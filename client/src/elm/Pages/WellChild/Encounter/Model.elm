module Pages.WellChild.Encounter.Model exposing (AssembledData, ECDPopupType(..), Model, Msg(..), Tab(..), VaccinationProgressDict, WarningPopupType(..), emptyModel)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (VaccineDose, WellChildMeasurements, WellChildVaccineType)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity)
import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , warningPopupState : Maybe WarningPopupType
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , warningPopupState = Nothing
    }


type Msg
    = CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SetWarningPopupState (Maybe WarningPopupType)
    | TriggerAcuteIllnessEncounter PersonId WellChildEncounterId
    | NavigateToActivity WellChildEncounterId WellChildActivity


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


type alias VaccinationProgressDict =
    Dict WellChildVaccineType (Dict VaccineDose NominalDate)
