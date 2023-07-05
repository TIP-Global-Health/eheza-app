module Pages.ChildScoreboard.Encounter.View exposing (view)

-- import Pages.ChildScoreboard.Activity.Utils exposing (activityCompleted, expectActivity)
-- import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
-- import Backend.Measurement.Model exposing (ChildScoreboardMeasurements)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardEncounter.Model exposing (ChildScoreboardEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetailsExtended, viewReportLink)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (activityCard, tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    -- @todo
    text "Pages.ChildScoreboard.Encounter.View"



-- view : Language -> NominalDate -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
-- view language currentDate id db model =
--     let
--         assembled =
--             generateAssembledData id db
--     in
--     viewWebData language (viewHeaderAndContent language currentDate db model) identity assembled
--
--
-- viewHeaderAndContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
-- viewHeaderAndContent language currentDate db model assembled =
--     let
--         header =
--             viewHeader language assembled
--
--         content =
--             viewContent language currentDate db model assembled
--
--         endEncounterDialog =
--             if model.showEndEncounterDialog then
--                 Just <|
--                     viewEndEncounterDialog language
--                         Translate.EndEncounterQuestion
--                         Translate.OnceYouEndTheEncounter
--                         (CloseEncounter assembled.id)
--                         (SetEndEncounterDialogState False)
--
--             else
--                 Nothing
--     in
--     div [ class "page-encounter ncd" ]
--         [ header
--         , content
--         , viewModal endEncounterDialog
--         ]
--
--
-- viewHeader : Language -> AssembledData -> Html Msg
-- viewHeader language assembled =
--     div
--         [ class "ui basic segment head" ]
--         [ h1
--             [ class "ui header" ]
--             [ text <|
--                 translate language <|
--                     Translate.IndividualEncounterLabel
--                         Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
--                         False
--             ]
--         , span
--             [ class "link-back"
--             , onClick <| SetActivePage <| UserPage <| ChildScoreboardParticipantPage InitiatorParticipantsPage assembled.participant.person
--             ]
--             [ span [ class "icon-back" ] []
--             , span [] []
--             ]
--         ]
--
--
-- viewContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
-- viewContent language currentDate db model assembled =
--     ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
--         :: viewMainPageContent language currentDate db assembled model
--     )
--         |> div [ class "ui unstackable items" ]
--
--
-- viewMainPageContent : Language -> NominalDate -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
-- viewMainPageContent language currentDate db assembled model =
--     let
--         ( completedActivities, pendingActivities ) =
--             List.filter (expectActivity currentDate assembled) getAllActivities
--                 |> List.partition (activityCompleted currentDate assembled)
--
--         pendingTabTitle =
--             translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities
--
--         completedTabTitle =
--             translate language <| Translate.ActivitiesCompleted <| List.length completedActivities
--
--         reportsTabTitle =
--             translate language Translate.ProgressReport
--
--         tabs =
--             div [ class "ui tabular menu" ]
--                 [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
--                 , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
--                 , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
--                 ]
--
--         viewCard activity =
--             activityCard language
--                 (Translate.ChildScoreboardActivityTitle activity)
--                 (getActivityIcon activity)
--                 (SetActivePage <| UserPage <| ChildScoreboardActivityPage assembled.id activity)
--
--         ( selectedActivities, emptySectionMessage ) =
--             case model.selectedTab of
--                 Pending ->
--                     ( pendingActivities, translate language Translate.NoActivitiesPending )
--
--                 Completed ->
--                     ( completedActivities, translate language Translate.NoActivitiesCompleted )
--
--                 Reports ->
--                     ( [], "" )
--
--         innerContent =
--             if model.selectedTab == Reports then
--                 div [ class "reports-wrapper" ]
--                     [ viewReportLink language
--                         Translate.ProgressReport
--                         (SetActivePage <|
--                             UserPage <|
--                                 ChildScoreboardProgressReportPage (InitiatorEncounterPage assembled.id)
--                         )
--                     ]
--
--             else
--                 div [ class "full content" ]
--                     [ div [ class "wrap-cards" ]
--                         [ div [ class "ui four cards" ] <|
--                             if List.isEmpty selectedActivities then
--                                 [ span [] [ text emptySectionMessage ] ]
--
--                             else
--                                 List.map viewCard selectedActivities
--                         ]
--                     ]
--
--         allowEndEncounter =
--             allowEndingEcounter pendingActivities
--
--         content =
--             div [ class "ui full segment" ]
--                 [ innerContent
--                 , viewEndEncounterButton language allowEndEncounter SetEndEncounterDialogState
--                 ]
--     in
--     [ tabs
--     , content
--     ]
--
--
-- allowEndingEcounter : List ChildScoreboardActivity -> Bool
-- allowEndingEcounter pendingActivities =
--     List.isEmpty pendingActivities
