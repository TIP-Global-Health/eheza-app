module Pages.NutritionProgressReport.View exposing (view)

-- import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
-- import Backend.Measurement.Model exposing (PrenatalMeasurements)
-- import Backend.Person.Model exposing (Person)
-- import Backend.Person.Utils exposing (ageInYears)
-- import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
-- import Date exposing (Interval(..))
-- import Gizra.Html exposing (emptyNode, showMaybe)
-- import List.Extra exposing (greedyGroupsOf)
-- import Maybe.Extra exposing (isJust, unwrap)
-- import Pages.ClinicalProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
-- import Pages.DemographicsReport.View exposing (viewHeader, viewItemHeading)
-- import Pages.Page exposing (Page(..), UserPage(..))
-- import Pages.PrenatalActivity.Utils exposing (calculateBmi)
-- import Pages.PrenatalEncounter.Model exposing (AssembledData)
-- import Pages.PrenatalEncounter.Utils exposing (..)
-- import Pages.Utils exposing (viewPhotoThumbFromPhotoUrl)
-- import PrenatalActivity.Model
--     exposing
--         ( PregnancyTrimester(..)
--         , allMedicalDiagnosis
--         , allObstetricalDiagnosis
--         , allRiskFactors
--         , allTrimesters
--         )
-- import PrenatalActivity.Utils
--     exposing
--         ( generateMedicalDiagnosisAlertData
--         , generateObstetricalDiagnosisAlertData
--         , generateRiskFactorAlertData
--         , getEncounterTrimesterData
--         )
-- import RemoteData exposing (RemoteData(..), WebData)
-- import Round

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Translate exposing (Language, TranslationId, translate)



-- import Utils.Html exposing (thumbnailImage)
-- import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NutritionEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate id db =
    div [ class "page-nutrition-progress-report" ] <|
        [ text "nutrition progress report" ]
