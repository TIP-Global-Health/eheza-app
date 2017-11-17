module Pages.Activity.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (getActivityIcon)
import Backend.Session.Model exposing (EditableSession)
import Date exposing (Date)
import EveryDict
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant(..), ParticipantId(..))
import Participant.Utils exposing (getParticipants, participantHasPendingActivity, getParticipantName, getParticipantAvatarThumb)
import Translate exposing (translate, Language)
import Utils.Html exposing (tabItem, thumbnailImage)


thumbnailDimensions : { height : Int, width : Int }
thumbnailDimensions =
    { width = 96
    , height = 96
    }


{-| Note that we don't "own" the activityType ... it just gets provided to us,
and is managed elsewhere.
-}
view : Language -> ActivityType -> EditableSession -> Model -> Html Msg
view language selectedActivity session model =
    let
        ( pendingParticipants, completedParticipants ) =
            getParticipants session.offlineSession
                |> EveryDict.partition (\id _ -> participantHasPendingActivity id selectedActivity session)

        activityDescription =
            div
                [ class "ui unstackable items" ]
                [ div [ class "item" ]
                    [ div [ class "ui image" ]
                        [ span [ class <| "icon-item icon-item-" ++ getActivityIcon selectedActivity ] [] ]
                    , div [ class "content" ]
                        [ p [] [ text <| translate language (Translate.ActivitiesHelp selectedActivity) ] ]
                    ]
                ]

        tabs =
            let
                pendingTabTitle =
                    EveryDict.size pendingParticipants
                        |> Translate.ActivitiesToComplete
                        |> translate language

                completedTabTitle =
                    EveryDict.size completedParticipants
                        |> Translate.ActivitiesCompleted
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
                            ( pendingParticipants, translate language Translate.NoParticipantsPendingForThisActivity )

                        Completed ->
                            ( completedParticipants, translate language Translate.NoParticipantsCompletedForThisActivity )

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
                    [ text <| translate language (Translate.ActivitiesTitle selectedActivity) ]
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
