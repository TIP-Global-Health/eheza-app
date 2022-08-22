module Pages.NCD.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , IndividualEncounterType(..)
        , IndividualParticipantInitiator(..)
        , emptyIndividualEncounterParticipant
        )
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NCD.Participant.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> IndividualParticipantInitiator -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw initiator db =
    text "Pages.NCD.Participant.View"
