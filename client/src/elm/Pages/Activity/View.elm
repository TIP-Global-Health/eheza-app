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
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..), thumbnailDimensions)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getParticipantName, getParticipantAvatarThumb)
import ParticipantManager.Utils exposing (filterParticipantsDict)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)
import Utils.Html exposing (emptyNode, tabItem)


view : BackendUrl -> String -> User -> Language -> Date -> ParticipantsDict -> Model -> List (Html Msg)
view backendUrl accessToken currentUser language currentDate participantsDict model =
    let
        selectedActivityIdentity =
            getActivityIdentity model.selectedActivity

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
                    let
                        name =
                            getParticipantName participant

                        imageSrc =
                            getParticipantAvatarThumb participant

                        imageView =
                            if String.isEmpty imageSrc then
                                span [ class "icon-participant" ] []
                            else
                                img
                                    [ src imageSrc
                                    , attribute "alt" name
                                    , style
                                        [ ( "height", (toString thumbnailDimensions.height) ++ "px" )
                                        , ( "width", (toString thumbnailDimensions.height) ++ "px" )
                                        ]
                                    ]
                                    []
                    in
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
                            , onClick <| SetSelectedParticipant <| Just ( participantId, participant )
                            ]
                            [ div
                                [ class "image" ]
                                [ imageView ]
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
