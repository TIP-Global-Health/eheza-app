module Pages.NextSteps.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..), emptySummaryByActivity)
import Activity.Utils exposing (getActivityIcon, getAllActivities, getParticipantCountForActivity)
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Session.Model exposing (EditableSession)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import LocalData
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (backFromSessionPage, viewEndEncounterDialog)
import Translate as Trans exposing (Language, translate)
import Utils.Html exposing (tabItem, viewModal)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Activity -> ( SessionId, EditableSession ) -> Model -> Html Msg
view language currentDate zscores childId originActivity ( sessionId, session ) model =
    text "Pages.NextSteps.View"



-- let
--     summary =
--         session.summaryByActivity
--             |> LocalData.withDefault emptySummaryByActivity
--
--     ( pendingActivities, completedActivities ) =
--         getAllActivities session.offlineSession
--             |> List.partition (\activity -> (getParticipantCountForActivity summary activity).pending > 0)
--
--     pendingTabTitle =
--         translate language <| Trans.ActivitiesToComplete <| List.length pendingActivities
--
--     completedTabTitle =
--         translate language <| Trans.ActivitiesCompleted <| List.length completedActivities
--
--     tabs =
--         div [ class "ui tabular menu" ]
--             [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
--             , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
--             ]
--
--     viewCard activity =
--         let
--             activityTitle =
--                 if activity == ChildActivity ChildFbf && session.offlineSession.session.clinicType == Achi then
--                     Trans.ActivitityTitleAchi
--
--                 else
--                     Trans.ActivitiesTitle activity
--         in
--         div
--             [ class "card" ]
--             [ div
--                 [ class "image"
--                 , onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId <| ActivityPage activity
--                 ]
--                 [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
--             , div
--                 [ class "content" ]
--                 [ p []
--                     [ text <| String.toUpper <| translate language activityTitle ]
--                 , div
--                     [ class "ui tiny progress" ]
--                     [ div
--                         [ class "label" ]
--                         [ getParticipantCountForActivity summary activity
--                             |> Trans.ReportCompleted
--                             |> translate language
--                             |> text
--                         ]
--                     ]
--                 ]
--             ]
--
--     endSessionDialog =
--         if model.showEndSessionDialog then
--             Just <|
--                 viewEndEncounterDialog language Trans.AreYouSure Trans.OnceYouEndYourGroupEncounter CloseSession (ShowEndSessionDialog False)
--
--         else
--             Nothing
--
--     ( selectedActivities, emptySectionMessage ) =
--         case model.selectedTab of
--             Pending ->
--                 ( pendingActivities, translate language Trans.NoActivitiesPending )
--
--             Completed ->
--                 ( completedActivities, translate language Trans.NoActivitiesCompleted )
--
--     goBackPage =
--         backFromSessionPage nurse session.offlineSession
--
--     endSessionAction =
--         if isCommunityHealthWorker nurse then
--             SetRedirectPage goBackPage
--
--         else
--             ShowEndSessionDialog True
--
--     endSessionButton =
--         div [ class "actions" ]
--             [ button
--                 [ class "ui fluid primary button"
--                 , onClick endSessionAction
--                 ]
--                 [ text <| translate language Trans.EndGroupEncounter ]
--             ]
-- in
-- div
--     [ class "wrap wrap-alt-2" ]
--     [ div
--         [ class "ui basic head segment" ]
--         [ h1
--             [ class "ui header" ]
--             [ text <| translate language Trans.Activities ]
--         , a
--             [ class "link-back"
--             , onClick <| SetRedirectPage goBackPage
--             ]
--             [ span [ class "icon-back" ] []
--             , span [] []
--             ]
--         , ul [ class "links-head" ]
--             [ li
--                 [ onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId AttendancePage ]
--                 [ a [] [ span [ class "icon-completed" ] [] ] ]
--             , li
--                 [ onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId ParticipantsPage ]
--                 [ a [] [ span [ class "icon-mother" ] [] ] ]
--             , li
--                 [ class "active" ]
--                 [ a [] [ span [ class "icon-measurements" ] [] ] ]
--             ]
--         ]
--     , tabs
--     , div
--         [ class "ui full segment" ]
--         [ div
--             [ class "full content" ]
--             [ div [ class "wrap-cards" ]
--                 [ div [ class "ui four cards" ] <|
--                     if List.isEmpty selectedActivities then
--                         [ span [] [ text emptySectionMessage ] ]
--
--                     else
--                         List.map viewCard selectedActivities
--                 ]
--             ]
--         , endSessionButton
--         ]
--     , viewModal endSessionDialog
--     ]
