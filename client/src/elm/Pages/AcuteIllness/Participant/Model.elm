module Pages.AcuteIllness.Participant.Model exposing (AcuteIllnessParticipantViewMode(..), Model, Msg(..), emptyModel)

import Backend.Model
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : AcuteIllnessParticipantViewMode }


emptyModel : Model
emptyModel =
    { viewMode = ManageIllnesses }


type Msg
    = MsgBackend Backend.Model.MsgIndexedDb
    | SetViewMode AcuteIllnessParticipantViewMode
    | SetActivePage Page


type AcuteIllnessParticipantViewMode
    = ManageIllnesses
    | ManageParticipants
    | RecordOutcome
