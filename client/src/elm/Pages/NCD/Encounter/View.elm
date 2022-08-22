module Pages.NCD.Encounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (NCDMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.NCDActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.NCD.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.NCD.Encounter.Model exposing (..)
import Pages.NCD.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    text "Pages.NCD.Encounter.View"
