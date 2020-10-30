module Pages.AcuteIllnessParticipant.Model exposing (AcuteIllnessParticipantViewMode(..), Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Model
import Date exposing (Date)
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : AcuteIllnessParticipantViewMode }


emptyModel : Model
emptyModel =
    { viewMode = ManageParticipants }


type Msg
    = MsgBackend Backend.Model.MsgIndexedDb
    | SetViewMode AcuteIllnessParticipantViewMode
    | SetActivePage Page


type AcuteIllnessParticipantViewMode
    = ManageParticipants
    | RecordOutcome
