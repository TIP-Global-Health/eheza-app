module Pages.ChildScoreboard.Activity.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> ChildScoreboardEncounterId
    -- -> ChildScoreboardActivity
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate id db model =
    -- @Todo
    emptyNode



-- view : Language -> NominalDate -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> Html Msg
-- view language currentDate id activity db model =
--     let
--         assembled =
--             generateAssembledData id db
--     in
--     viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled
--
--
-- viewHeaderAndContent : Language -> NominalDate -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
-- viewHeaderAndContent language currentDate id activity db model assembled =
--     div [ class "page-activity ncd" ] <|
--         [ viewHeader language id activity
--         , viewContent language currentDate activity db model assembled
--         ]
--
--
-- viewHeader : Language -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> Html Msg
-- viewHeader language id activity =
--     div
--         [ class "ui basic segment head" ]
--         [ h1
--             [ class "ui header" ]
--             [ text <| translate language <| Translate.ChildScoreboardActivityTitle activity ]
--         , span
--             [ class "link-back"
--             , onClick <| SetActivePage <| UserPage <| ChildScoreboardEncounterPage id
--             ]
--             [ span [ class "icon-back" ] []
--             , span [] []
--             ]
--         ]
--
--
-- viewContent : Language -> NominalDate -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
-- viewContent language currentDate activity db model assembled =
--     div [ class "ui unstackable items" ] <|
--         ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
--             :: viewActivity language currentDate activity assembled db model
--         )
--
--
-- viewActivity : Language -> NominalDate -> ChildScoreboardActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
-- viewActivity language currentDate activity assembled db model =
--     case activity of
--         DangerSigns ->
--             viewDangerSignsContent language currentDate assembled model.dangerSignsData
--
--         Examination ->
--             viewExaminationContent language currentDate assembled model.examinationData
--
--         FamilyPlanning ->
--             viewFamilyPlanningContent language currentDate assembled model.familyPlanningData
--
--         Laboratory ->
--             viewLaboratoryContent language currentDate assembled model.laboratoryData
--
--         MedicalHistory ->
--             viewMedicalHistoryContent language currentDate assembled model.medicalHistoryData
--
--         SymptomReview ->
--             viewSymptomReviewContent language currentDate assembled model.symptomReviewData
--
--         OutsideCare ->
--             viewOutsideCareContent language currentDate assembled model.medicalHistoryData.outsideCareForm
--
--         NextSteps ->
--             viewNextStepsContent language currentDate assembled model.nextStepsData
