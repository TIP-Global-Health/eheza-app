module Pages.FamilyNutrition.Encounter.Model exposing (AhezaData, AssembledData, DialogType(..), FamilyMember(..), Model, Msg(..), MuacData, PhotoData, Tab(..), emptyAhezaData, emptyModel, emptyMuacData, emptyPhotoData)

import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model exposing (FamilyEncounterParticipant)
import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity)
import Backend.FamilyNutritionEncounter.Model exposing (FamilyNutritionEncounter)
import Backend.Measurement.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (AhezaForm, AnthropometricMeasurement, DropZoneFile, MuacForm, PhotoForm, emptyAhezaForm, emptyMuacForm, emptyPhotoForm)
import Pages.Page exposing (Page)


type alias Model =
    { ahezaData : AhezaData
    , muacData : MuacData
    , photoData : PhotoData
    , selectedActivity : Maybe FamilyNutritionActivity
    , selectedTab : Tab
    , dialogState : Maybe DialogType
    , selectedFamilyMember : Maybe FamilyMember

    -- The measurement that was entered outside the range it can take, named on
    -- a warning until the nurse closes it. This encounter shows no range above
    -- the input, so the warning is the only place it is said.
    , measurementOutOfRangePopupState : List AnthropometricMeasurement
    }


emptyModel : Model
emptyModel =
    { ahezaData = emptyAhezaData
    , muacData = emptyMuacData
    , photoData = emptyPhotoData
    , selectedActivity = Nothing
    , selectedTab = Pending
    , dialogState = Nothing
    , selectedFamilyMember = Just FamilyMemberMother
    , measurementOutOfRangePopupState = []
    }


type Msg
    = CloseEncounter FamilyNutritionEncounterId
    | DropZoneComplete DropZoneFile
    | SaveAhezaChild PersonId (Maybe ( AhezaChildId, AhezaChild ))
    | SaveAhezaMother PersonId (Maybe ( AhezaMotherId, AhezaMother ))
    | PreSaveMuacChild PersonId (Maybe ( FamilyNutritionMuacChildId, FamilyNutritionMuacChild ))
    | PreSaveMuacMother PersonId (Maybe ( FamilyNutritionMuacMotherId, FamilyNutritionMuacMother ))
    | SaveMuacChild PersonId (Maybe ( FamilyNutritionMuacChildId, FamilyNutritionMuacChild ))
    | SaveMuacMother PersonId (Maybe ( FamilyNutritionMuacMotherId, FamilyNutritionMuacMother ))
    | SavePhoto PersonId (Maybe ( FamilyNutritionPhotoId, FamilyNutritionPhoto ))
    | SetActivePage Page
    | SetAheza String
    | SetAhezaDistributionReason String
    | SetDialogState (Maybe DialogType)
    | SetMeasurementOutOfRangePopupState (List AnthropometricMeasurement)
    | SetMuac String
    | SetSelectedActivity (Maybe FamilyNutritionActivity)
    | SetSelectedFamilyMember (Maybe FamilyMember)
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


type alias PhotoData =
    { form : PhotoForm
    }


emptyPhotoData : PhotoData
emptyPhotoData =
    PhotoData emptyPhotoForm
