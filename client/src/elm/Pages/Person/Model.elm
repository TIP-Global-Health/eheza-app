module Pages.Person.Model exposing (Model, Msg(..), emptyModel)

import Backend.Person.Form exposing (PersonForm)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)


type alias Model =
    PersonForm


type Msg
    = MsgForm Form.Msg
    | ResetCreateForm
    | SetActivePage Page
    | DropZoneComplete DropZoneFile


emptyModel : Model
emptyModel =
    Backend.Person.Form.emptyForm
