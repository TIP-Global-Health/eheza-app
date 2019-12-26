module Pages.Person.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Person.Form exposing (PersonForm)
import Backend.Person.Model exposing (RegistrationInitiator)
import Date exposing (Date)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)


type alias Model =
    { form : PersonForm
    , isDateSelectorOpen : Bool
    }


type Msg
    = -- The personId, if provided, is a person we should offer to create
      -- a relationship with.
      MsgForm (Maybe PersonId) RegistrationInitiator Form.Msg
    | ResetCreateForm
    | SetActivePage Page
    | DropZoneComplete (Maybe PersonId) RegistrationInitiator DropZoneFile
    | ToggleDateSelector
    | DateSelected (Maybe PersonId) RegistrationInitiator Date


emptyModel : Model
emptyModel =
    { form = Backend.Person.Form.emptyForm
    , isDateSelectorOpen = False
    }
