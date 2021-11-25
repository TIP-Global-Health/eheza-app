module Pages.Person.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Person.Form exposing (PersonForm)
import Backend.Person.Model exposing (Initiator, ParticipantDirectoryOperation)
import Date exposing (Date)
import Form
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)


type alias Model =
    { form : PersonForm
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyCreateModel : Model
emptyCreateModel =
    { form = Backend.Person.Form.emptyCreateForm
    , dateSelectorPopupState = Nothing
    }


emptyEditModel : Model
emptyEditModel =
    { form = Backend.Person.Form.emptyEditForm
    , dateSelectorPopupState = Nothing
    }


type alias DateSelectorConfig msg =
    { select : Date -> msg
    , close : msg
    , dateFrom : Date
    , dateTo : Date
    }


type Msg
    = MsgForm ParticipantDirectoryOperation Initiator Form.Msg
    | ResetCreateForm
    | ResetEditForm
    | SetActivePage Page
    | DropZoneComplete ParticipantDirectoryOperation Initiator DropZoneFile
    | DateSelected ParticipantDirectoryOperation Initiator Date
    | SetDateSelectorState (Maybe (DateSelectorConfig Msg))
