module Pages.Activity.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (getActivityIdentity, hasPendingChildActivity, hasPendingMotherActivity)
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import Examination.Utils exposing (getLastExaminationFromChild)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.View
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getParticipantName)
import ParticipantManager.Utils exposing (filterParticipantsDict)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)
import Utils.Html exposing (emptyNode, tabItem)


view : BackendUrl -> String -> User -> Language -> Date -> ParticipantsDict -> Model -> List (Html Msg)
view backendUrl accessToken currentUser language currentDate participantsDict model =
    let
        selectedActivityIdentity =
            getActivityIdentity model.selectedActivity

        activityDescription =
            let
                description =
                    case selectedActivityIdentity.activityType of
                        Child ChildPicture ->
                            Trans.ActivityChildPhotoDescription

                        Child Height ->
                            Trans.ActivityHeightDescription

                        Child Muac ->
                            Trans.ActivityMuacDescription

                        Child NutritionSigns ->
                            Trans.ActivityNutritionSigns

                        Child Weight ->
                            Trans.ActivityWeightDescription

                        Mother FamilyPlanning ->
                            Trans.ActivityFamilyPlanningDescription

                        _ ->
                            Trans.EmptyString
            in
                div
                    [ class "ui unstackable items" ]
                    [ div [ class "item" ]
                        [ div [ class "ui image" ]
                            [ span [ class <| "icon-item icon-item-" ++ selectedActivityIdentity.icon ] [] ]
                        , div [ class "content" ]
                            [ p [] [ text <| translate language description ] ]
                        ]
                    ]

        participantsWithPendingActivity =
            participantsDict
                |> Dict.filter
                    (\participantId participant ->
                        case participant.info of
                            ParticipantChild child ->
                                case selectedActivityIdentity.activityType of
                                    Child activityType ->
                                        not <| hasPendingChildActivity currentDate activityType child

                                    Mother _ ->
                                        False

                            ParticipantMother mother ->
                                case selectedActivityIdentity.activityType of
                                    Child _ ->
                                        False

                                    Mother activityType ->
                                        not <| hasPendingMotherActivity currentDate activityType mother
                    )

        participantsWithCompletedActivity =
            participantsDict
                |> Dict.filter
                    (\participantId participant ->
                        case participant.info of
                            ParticipantChild child ->
                                case selectedActivityIdentity.activityType of
                                    Child activityType ->
                                        hasPendingChildActivity currentDate activityType child

                                    Mother _ ->
                                        False

                            ParticipantMother mother ->
                                case selectedActivityIdentity.activityType of
                                    Child _ ->
                                        False

                                    Mother activityType ->
                                        hasPendingMotherActivity currentDate activityType mother
                    )

        tabs =
            let
                pendingTabTitle =
                    translate language <| Trans.ActivitiesToComplete <| Dict.size participantsWithPendingActivity

                completedTabTitle =
                    translate language <| Trans.ActivitiesCompleted <| Dict.size participantsWithCompletedActivity
            in
                div [ class "ui tabular menu" ]
                    [ tabItem pendingTabTitle (model.selectedTab == Pending) (SetSelectedTab Pending)
                    , tabItem completedTabTitle (model.selectedTab == Completed) (SetSelectedTab Completed)
                    ]

        participants =
            let
                selectedParticipants =
                    case model.selectedTab of
                        Pending ->
                            participantsWithPendingActivity

                        Completed ->
                            participantsWithCompletedActivity

                viewParticipantCard maybeSelectedParticipant ( participantId, participant ) =
                    div
                        [ classList
                            [ ( "participant card", True )
                            , ( "active"
                              , case maybeSelectedParticipant of
                                    Just ( id, _ ) ->
                                        id == participantId

                                    Nothing ->
                                        False
                              )
                            ]
                        ]
                        [ div
                            [ class "image"
                            , onClick <| SetSelectedParticipant <| Just ( participantId, participant )
                            ]
                            [ span [ class "icon-participant" ] [] ]
                        , div [ class "content" ]
                            [ p [] [ text <| getParticipantName participant ] ]
                        ]
            in
                div
                    [ class "ui participant segment" ]
                    [ div [ class "ui four participant cards" ] <|
                        List.map (viewParticipantCard model.selectedParticipant) <|
                            Dict.toList selectedParticipants
                    ]

        measurementsForm =
            case model.selectedParticipant of
                Just ( participantId, participant ) ->
                    case participant.info of
                        ParticipantChild child ->
                            Html.map (MsgMeasurement ( participantId, participant )) <|
                                Measurement.View.viewChild backendUrl accessToken currentUser language ( participantId, child ) (getLastExaminationFromChild child) (Just model.selectedActivity) model.measurements

                        ParticipantMother mother ->
                            Html.map (MsgMeasurement ( participantId, participant )) <|
                                Measurement.View.viewMother backendUrl accessToken currentUser language (Just model.selectedActivity) model.measurements

                Nothing ->
                    emptyNode
    in
        [ activityDescription, tabs, participants, measurementsForm ]



-- ++ [ Html.map MsgMeasurement <|
--         Measurement.View.viewChild backendUrl accessToken currentUser language ( childId, child ) (getLastExaminationFromChild child) model.selectedActivity model.measurements
--    ]
-- ++ [ Html.map MsgMeasurement <|
--         Measurement.View.viewMother backendUrl accessToken currentUser language model.selectedActivity model.measurements
--    ]
-- let
--     allActivityList =
--         getActivityList currentDate model.participantTypeFilter participants
--
--     pendingActivities =
--         List.filter (\activity -> (Tuple.first activity.totals > 0)) allActivityList
--
--     noPendingActivities =
--         List.filter (\activity -> (Tuple.first activity.totals == 0)) allActivityList
--
--     tabItem tab activitiesList =
--         let
--             tabTitle =
--                 case tab of
--                     Pending ->
--                         Trans.ActivitiesToComplete
--
--                     Completed ->
--                         Trans.ActivitiesCompleted
--
--             tabClass tab =
--                 [ ( "item", True )
--                 , ( "active", model.activeTab == tab )
--                 ]
--         in
--             a
--                 [ classList <| tabClass tab
--                 , onClick <| SetActiveTab tab
--                 ]
--                 [ text <| translate language <| tabTitle <| List.length activitiesList ]
--
--     tabs =
--         div [ class "ui tabular menu" ]
--             [ tabItem Pending pendingActivities
--             , tabItem Completed noPendingActivities
--             ]
--
--     viewCard language identity =
--         div
--             [ class "card" ]
--             [ div
--                 [ class "image" ]
--                 [ span [ class <| "icon-task icon-task-" ++ identity.activity.icon ] [] ]
--             , div
--                 [ class "content" ]
--                 [ p [] [ text <| String.toUpper identity.activity.name ]
--                 , div
--                     [ class "ui tiny progress" ]
--                     [ div
--                         [ class "label" ]
--                         [ text <| translate language <| Trans.ReportCompleted identity.totals ]
--                     ]
--                 ]
--             ]
--
--     selectedActivies =
--         case model.activeTab of
--             Pending ->
--                 pendingActivities
--
--             Completed ->
--                 noPendingActivities
-- in
--     [ tabs
--     , div
--         [ class "ui full segment" ]
--         [ div [ class "content" ]
--             [ div [ class "ui four cards" ] <|
--                 List.map (viewCard language) selectedActivies
--             ]
--         , div [ class "actions" ]
--             [ button
--                 [ class "ui fluid button" ]
--                 [ text <| translate language Trans.EndSession ]
--             ]
--         ]
--     ]
