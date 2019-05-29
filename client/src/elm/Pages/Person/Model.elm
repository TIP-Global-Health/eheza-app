module Pages.Person.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Person.Form exposing (PersonForm)
import Date exposing (Date)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)


type alias Model =
    { form : PersonForm
    , selectedDate : Maybe Date
    , isDateSelectorOpen : Bool
    }


type Msg
    = -- The personId, if provided, is a person we should offer to create
      -- a relationship with.
      MsgForm (Maybe PersonId) Form.Msg
    | ResetCreateForm
    | SetActivePage Page
    | DropZoneComplete (Maybe PersonId) DropZoneFile
    | ToggleDateSelector
    | DateSelected (Maybe PersonId) Date


emptyModel : Model
emptyModel =
    { form = Backend.Person.Form.emptyForm
    , selectedDate = Nothing
    , isDateSelectorOpen = False
    }
