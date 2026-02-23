module Pages.FamilyNutrition.Encounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterParticipant)
import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity)
import Backend.FamilyNutritionEncounter.Model exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (AhezaForm, MuacForm, emptyAhezaForm, emptyMuacForm)
import Pages.Page exposing (Page)


type alias Model =
    { ahezaData : AhezaData
    , muacData : MuacData
    , selectedActivity : Maybe FamilyNutritionActivity
    , selectedTab : Tab
    , dialogState : Maybe DialogType
    , selectedFamilyMember : FamilyMember
    }


emptyModel : Model
emptyModel =
    { ahezaData = emptyAhezaData
    , muacData = emptyMuacData
    , selectedActivity = Nothing
    , selectedTab = Pending
    , dialogState = Nothing
    , selectedFamilyMember = FamilyMemberMother
    }


type Msg
    = CloseEncounter FamilyNutritionEncounterId
    | SaveAhezaChild PersonId (Maybe ( AhezaChildId, AhezaChild ))
    | SaveAhezaMother PersonId (Maybe ( AhezaMotherId, AhezaMother ))
    | SaveMuacChild PersonId (Maybe ( FamilyNutritionMuacChildId, FamilyNutritionMuacChild ))
    | SaveMuacMother PersonId (Maybe ( FamilyNutritionMuacMotherId, FamilyNutritionMuacMother ))
    | SetActivePage Page
    | SetAheza String
    | SetDialogState (Maybe DialogType)
    | SetMuac String
    | SetSelectedActivity (Maybe FamilyNutritionActivity)
    | SetSelectedFamilyMember FamilyMember
    | SetSelectedTab Tab


type FamilyMember
    = FamilyMemberMother
    | FamilyMemberChild PersonId


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


type alias AhezaData =
    { form : AhezaForm
    }


emptyAhezaData : AhezaData
emptyAhezaData =
    AhezaData emptyAhezaForm


type alias MuacData =
    { form : MuacForm
    }


emptyMuacData : MuacData
emptyMuacData =
    MuacData emptyMuacForm
