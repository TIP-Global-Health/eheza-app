module Pages.Person.Model exposing (Model, Msg(..), emptyCreateModel, emptyEditModel)

import Backend.Entities exposing (PersonId)
import Backend.Person.Form exposing (PersonForm)
import Backend.Person.Model exposing (Initiator, ParticipantDirectoryOperation, Person)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)
import SyncManager.Model exposing (Site)


type alias Model =
    { form : PersonForm
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , dialogState : Maybe ( Person, Msg )
    }


emptyCreateModel : Site -> Model
emptyCreateModel site =
    { form = Backend.Person.Form.emptyCreateForm site
    , dateSelectorPopupState = Nothing
    , dialogState = Nothing
    }


emptyEditModel : Site -> Model
emptyEditModel site =
    { form = Backend.Person.Form.emptyEditForm site
    , dateSelectorPopupState = Nothing
    , dialogState = Nothing
    }


type Msg
    = MsgForm ParticipantDirectoryOperation Initiator Form.Msg
    | ResetCreateForm
    | ResetEditForm
    | SetActivePage Page
    | DropZoneComplete ParticipantDirectoryOperation Initiator DropZoneFile
    | DateSelected ParticipantDirectoryOperation Initiator Date
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetDialogState (Maybe ( Person, Msg ))
    | PostPerson (Maybe PersonId) Initiator Person
