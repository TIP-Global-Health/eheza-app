module Pages.Prenatal.Participant.Model exposing (Model, Msg(..), ShownPregnancy, emptyModel)

import Backend.Entities exposing (..)
import Backend.Model
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
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


type Msg
    = MsgBackend Backend.Model.MsgIndexedDb
    | SetActivePage Page
    | CloseWarningPopup


emptyModel : Model
emptyModel =
    { showWarningPopup = True }
