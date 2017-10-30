module Pages.Activity.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (getActivityIdentity)
import Backend.Session.Model exposing (EditableSession)
import Date exposing (Date)
import EveryDict
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Measurement.View
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant(..), ParticipantId(..), ParticipantTypeFilter(..))
import Participant.Utils exposing (getParticipants, participantHasPendingActivity, getParticipantName, getParticipantAvatarThumb)
import Restful.Endpoint exposing (toEntityId)
import Translate as Trans exposing (translate, Language)
import Utils.Html exposing (tabItem, thumbnailImage)


thumbnailDimensions : { height : Int, width : Int }
thumbnailDimensions =
    { width = 96
    , height = 96
    }


view : Language -> Date -> EditableSession -> Model -> Html Msg
view language currentDate session model =
    let
        selectedActivityIdentity =
            getActivityIdentity model.selectedActivity

        ( pendingParticipants, completedParticipants ) =
            getParticipants session.offlineSession
                |> EveryDict.partition (\id _ -> participantHasPendingActivity id model.selectedActivity session)

        activityDescription =
            div
                [ class "ui unstackable items" ]
                [ div [ class "item" ]
                    [ div [ class "ui image" ]
                        [ span [ class <| "icon-item icon-item-" ++ selectedActivityIdentity.icon ] [] ]
                    , div [ class "content" ]
                        [ p [] [ text <| translate language (Trans.ActivitiesHelp model.selectedActivity) ] ]
                    ]
                ]

        tabs =
            let
                pendingTabTitle =
                    EveryDict.size pendingParticipants
                        |> Trans.ActivitiesToComplete
                        |> translate language

                completedTabTitle =
                    EveryDict.size completedParticipants
                        |> Trans.ActivitiesCompleted
                        |> translate language
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
                            ( pendingParticipants, translate language Trans.PendingSectionEmpty )

                        Completed ->
                            ( completedParticipants, translate language Trans.CompletedSectionEmpty )

                viewParticipantCard ( participantId, participant ) =
                    let
                        name =
                            getParticipantName participant

                        imageSrc =
                            getParticipantAvatarThumb participant

                        imageView =
                            thumbnailImage participant imageSrc name thumbnailDimensions.height thumbnailDimensions.width
                    in
                        div
                            [ classList
                                [ ( "participant card", True )
                                , ( "active", Just participantId == model.selectedParticipant )
                                ]
                            , onClick <|
                                SetSelectedParticipant <|
                                    Just participantId
                            ]
                            [ div
                                [ class "image" ]
                                [ imageView ]
                            , div [ class "content" ]
                                [ p [] [ text <| getParticipantName participant ] ]
                            ]

                participantsCards =
                    if EveryDict.size selectedParticipants == 0 then
                        [ span [] [ text emptySectionMessage ] ]
                    else
                        selectedParticipants
                            |> EveryDict.toList
                            |> List.sortBy (\( _, participant ) -> getParticipantName participant)
                            |> List.map viewParticipantCard
            in
                div
                    [ class "ui participant segment" ]
                    [ div [ class "ui four participant cards" ]
                        participantsCards
                    ]

        measurementsForm =
            case model.selectedParticipant of
                Just (ParticipantChildId childId) ->
                    Debug.crash "implement"

                Just (ParticipantMotherId motherId) ->
                    Debug.crash "implement"

                Nothing ->
                    emptyNode

        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ text identity.name ]
                , a
                    [ class "link-back"
                    , Debug.crash "redo"

                    {-
                       , onClick <|
                           MsgPagesActivity <|
                               Pages.Activity.Model.SetRedirectPage <|
                                   App.PageType.Activities
                    -}
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
        div
            [ class "wrap" ]
            [ header
            , activityDescription
            , tabs
            , participants
            , measurementsForm
            ]
