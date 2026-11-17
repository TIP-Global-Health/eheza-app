module Pages.Prenatal.Participant.Model exposing (ChwVisitButtons, Model, Msg(..), ShownPregnancy, emptyModel)

import Backend.Entities exposing (..)
import Backend.Model
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType)
import Pages.Page exposing (Page)


type alias Model =
    { showWarningPopup : Bool
    }


{-| The pregnancy the participant page shows, with its encounters newest first.
-}
type alias ShownPregnancy =
    { sessionId : IndividualEncounterParticipantId
    , encounters : List ( PrenatalEncounterId, PrenatalEncounter )
    , keptForPostpartumVisit : Bool
    }


{-| State of the three CHW visit buttons. An enabled button opens
`visitToResume` when there is one, and starts a new visit otherwise.
-}
type alias ChwVisitButtons =
    { visitToResume : Maybe PrenatalEncounterId
    , firstVisitEnabled : Bool
    , subsequentVisitType : PrenatalEncounterType
    , subsequentVisitEnabled : Bool
    , postpartumVisitEnabled : Bool
    }


type Msg
    = MsgBackend Backend.Model.MsgIndexedDb
    | SetActivePage Page
    | CloseWarningPopup


emptyModel : Model
emptyModel =
    { showWarningPopup = True }
