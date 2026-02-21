module Pages.FamilyNutrition.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterParticipant)
import Backend.FamilyNutritionEncounter.Model exposing (..)
import Backend.Measurement.Model exposing (FamilyNutritionMeasurements)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , dialogState : Maybe DialogType
    , selectedFamilyMember : FamilyMemberPage
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , dialogState = Nothing
    , selectedFamilyMember = MotherPage
    }


type Msg
    = CloseEncounter FamilyNutritionEncounterId
    | SetActivePage Page
    | SetSelectedTab Tab
    | SetDialogState (Maybe DialogType)
    | SetSelectedFamilyMember FamilyMemberPage


type FamilyMemberPage
    = MotherPage
    | ChildPage PersonId


type Tab
    = Completed
    | Pending
    | Reports


type DialogType
    = DialogEndEncounter


type alias AssembledData =
    { id : FamilyNutritionEncounterId
    , encounter : FamilyNutritionEncounter
    , participant : FamilyEncounterParticipant
    , person : Person
    , measurements : FamilyNutritionMeasurements
    , previousMeasurementsWithDates : List ( NominalDate, ( FamilyNutritionEncounterId, FamilyNutritionMeasurements ) )
    , children : List ( PersonId, Person )
    }
