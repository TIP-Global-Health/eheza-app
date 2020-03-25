module Pages.Person.Model exposing (Model, Msg(..), emptyCreateModel, emptyEditModel)

import Backend.Entities exposing (..)
import Backend.Person.Form exposing (PersonForm)
import Backend.Person.Model exposing (ParticipantDirectoryOperation, RegistrationInitiator)
import Date exposing (Date)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)


type alias Model =
    { form : PersonForm
    , isDateSelectorOpen : Bool
    }


type Msg
    = MsgForm ParticipantDirectoryOperation RegistrationInitiator Form.Msg
    | ResetCreateForm
    | ResetEditForm
    | SetActivePage Page
    | DropZoneComplete ParticipantDirectoryOperation RegistrationInitiator DropZoneFile
    | ToggleDateSelector
    | DateSelected ParticipantDirectoryOperation RegistrationInitiator Date


emptyCreateModel : Model
emptyCreateModel =
    { form = Backend.Person.Form.emptyCreateForm
    , isDateSelectorOpen = False
    }


emptyEditModel : Model
emptyEditModel =
    { form = Backend.Person.Form.emptyCreateForm
    , isDateSelectorOpen = False
    }
