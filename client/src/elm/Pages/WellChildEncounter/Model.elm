module Pages.WellChildEncounter.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (VaccineDose, VaccineType, WellChildMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity)
import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
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
    Dict VaccineType (Dict VaccineDose NominalDate)
