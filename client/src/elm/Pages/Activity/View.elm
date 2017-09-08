module Pages.Activity.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (getActivityIdentity, hasPendingChildActivity, hasPendingMotherActivity)
import Date exposing (Date)
import Dict
import Examination.Utils exposing (getLastExaminationFromChild)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Measurement.View
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..), thumbnailDimensions)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getParticipantName, getParticipantAvatarThumb)
import Translate as Trans exposing (translate, Language)
import Utils.Html exposing (emptyNode, tabItem, thumbnailImage)


view : Language -> Date -> ParticipantsDict -> Model -> List (Html Msg)
view language currentDate participantsDict model =
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
                                        hasPendingChildActivity child activityType

                                    Mother _ ->
                                        False

                            ParticipantMother mother ->
                                case selectedActivityIdentity.activityType of
                                    Child _ ->
                                        False

                                    Mother activityType ->
                                        hasPendingMotherActivity mother activityType
                    )

        participantsWithCompletedActivity =
            participantsDict
                |> Dict.filter
                    (\participantId participant ->
                        case participant.info of
                            ParticipantChild child ->
                                case selectedActivityIdentity.activityType of
                                    Child activityType ->
                                        not <| hasPendingChildActivity child activityType

                                    Mother _ ->
                                        False

                            ParticipantMother mother ->
                                case selectedActivityIdentity.activityType of
                                    Child _ ->
                                        False

                                    Mother activityType ->
                                        not <| hasPendingMotherActivity mother activityType
                    )

        activityDescription =
            let
                description =
                    case selectedActivityIdentity.activityType of
                        Child ChildPicture ->
                            Trans.ActivitiesPhotoHelp

                        Child Height ->
                            Trans.ActivitiesHeightHelp

                        Child Muac ->
                            Trans.ActivitiesMuacHelp

                        Child NutritionSigns ->
                            Trans.ActivitiesNutritionSignsHelp

                        Child Weight ->
                            Trans.ActivitiesWeightHelp

                        Child ProgressReport ->
                            Trans.ActivitiesProgressReportHelp

                        Mother FamilyPlanning ->
                            Trans.ActivitiesFamilyPlanningSignsHelp
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
                    [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                    , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                    ]

        participants =
            let
                ( selectedParticipants, emptySectionMessage ) =
                    case model.selectedTab of
                        Pending ->
                            ( participantsWithPendingActivity, translate language Trans.PendingSectionEmpty )

                        Completed ->
                            ( participantsWithCompletedActivity, translate language Trans.CompletedSectionEmpty )

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
                                thumbnailImage imageSrc name thumbnailDimensions.height thumbnailDimensions.width
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

                participantsCards =
                    if Dict.size selectedParticipants == 0 then
                        [ span [] [ text emptySectionMessage ] ]
                    else
                        List.map (viewParticipantCard model.selectedParticipant) <|
                            List.sortBy
                                (\( _, participant ) ->
                                    getParticipantName participant
                                )
                            <|
                                Dict.toList selectedParticipants
            in
                div
                    [ class "ui participant segment" ]
                    [ div [ class "ui four participant cards" ]
                        participantsCards
                    ]

        measurementsForm =
            case model.selectedParticipant of
                Just ( participantId, participant ) ->
                    case participant.info of
                        ParticipantChild child ->
                            Html.map (MsgMeasurement ( participantId, participant )) <|
                                Measurement.View.viewChild language ( participantId, child ) (getLastExaminationFromChild child) (Just model.selectedActivity) model.measurements

                        ParticipantMother mother ->
                            Html.map (MsgMeasurement ( participantId, participant )) <|
                                Measurement.View.viewMother language (Just model.selectedActivity) model.measurements

                Nothing ->
                    emptyNode
    in
        [ activityDescription, tabs, participants, measurementsForm ]
