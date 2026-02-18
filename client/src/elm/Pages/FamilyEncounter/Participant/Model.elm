module Pages.FamilyEncounter.Participant.Model exposing (Model, Msg(..), MotherSelection(..), RegistrationMode(..), ChildFormData, MotherFormData, emptyModel, maxChildren)

import Backend.Entities exposing (..)
import Backend.Person.Form
import Backend.Person.Model exposing (Initiator, Person)
import Components.PatientsSearchForm.Model
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)
import SyncManager.Model


maxChildren : Int
maxChildren =
    5


type alias Model =
    { searchForm : Components.PatientsSearchForm.Model.Model
    , selectedMother : Maybe MotherSelection
    , selectedChildren : List PersonId
    , registrationMode : RegistrationMode
    , motherForm : Maybe MotherFormData
    , childForm : Maybe ChildFormData
    }


type MotherSelection
    = ExistingMother PersonId Person
    | NewMother


type RegistrationMode
    = SearchMode
    | RegisterMotherMode
    | RegisterChildMode


type alias MotherFormData =
    { form : Backend.Person.Form.PersonForm
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


type alias ChildFormData =
    { form : Backend.Person.Form.PersonForm
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyModel : SyncManager.Model.Site -> Model
emptyModel site =
    { searchForm = Components.PatientsSearchForm.Model.emptyModel
    , selectedMother = Nothing
    , selectedChildren = []
    , registrationMode = SearchMode
    , motherForm = Nothing
    , childForm = Nothing
    }


type Msg
    = SetActivePage Page
    | MsgPatientsSearchForm Components.PatientsSearchForm.Model.Msg
    | SelectMother PersonId Person
    | DeselectMother
    | RegisterNewMother
    | AddChild PersonId Person
    | RemoveChild PersonId
    | RegisterNewChild
    | BackToSearch
    | MsgMotherForm Form.Msg
    | MsgChildForm Form.Msg
    | DropZoneCompleteMotherPhoto DropZoneFile
    | DropZoneCompleteChildPhoto DropZoneFile
    | DateSelectedMother Date
    | DateSelectedChild Date
    | SetMotherDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetChildDateSelectorState (Maybe (DateSelectorConfig Msg))
    | CreateEncounter
