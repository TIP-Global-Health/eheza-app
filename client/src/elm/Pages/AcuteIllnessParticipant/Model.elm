module Pages.AcuteIllnessParticipant.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Model
import Date exposing (Date)
import Pages.Page exposing (Page)


type alias Model =
    {}


type Msg
    = MsgBackend Backend.Model.MsgIndexedDb
    | RecordIllnessOutcome
    | SetActivePage Page


emptyModel : Model
emptyModel =
    {}
