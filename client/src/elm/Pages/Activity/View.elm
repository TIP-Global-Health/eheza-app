module Pages.Activity.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (getActivityIcon, onlyCheckedIn, childHasPendingActivity, motherHasPendingActivity)
import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (EditableSession)
import EveryDict exposing (EveryDict)
import EveryDictList
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant(..), ParticipantId(..))
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
view language selectedActivity =
    -- It's awkward to try to deal with children and mothers together, and we
    -- know which we want, based on the activity type. So, we specialize!
    case selectedActivity of
        ChildActivity childActivity ->
            viewActivity childConfig language childActivity

        MotherActivity motherActivity ->
            viewActivity motherConfig language motherActivity


{-| TODO: At some point, we should be able to get rid of the `ParticipantId`
and `Participant` types entirely ... in this transitional stage, we have
some functions that want them.
-}
type alias Config activity id participant =
    { hasPendingActivity : id -> activity -> EditableSession -> Bool
    , getParticipants : EditableSession -> EveryDict id participant
    , wrapActivityType : activity -> ActivityType
    , wrapParticipantId : id -> ParticipantId
    , getAvatarThumb : participant -> String
    , getName : participant -> String
    , getBirthDate : participant -> NominalDate
    , iconClass : String
    }


childConfig : Config ChildActivityType ChildId Child
childConfig =
    { hasPendingActivity = childHasPendingActivity
    , getParticipants = \session -> session.offlineSession.children
    , wrapActivityType = ChildActivity
    , wrapParticipantId = ParticipantChildId
    , getAvatarThumb = .image
    , getName = .name
    , getBirthDate = .birthDate
    , iconClass = "child"
    }


motherConfig : Config MotherActivityType MotherId Mother
motherConfig =
    -- TODO: getParticipants is inefficient ... should make the children and
    -- mothers match, and either pre-sort in EveryDictList or sort each time in
    -- EveryDict
    { hasPendingActivity = motherHasPendingActivity
    , getParticipants = \session -> session.offlineSession.mothers |> EveryDictList.toList |> EveryDict.fromList
    , wrapActivityType = MotherActivity
    , wrapParticipantId = ParticipantMotherId
    , getAvatarThumb = .image
    , getName = .name
    , getBirthDate = .birthDate
    , iconClass = "mother"
    }


viewActivity : Config activity id participant -> Language -> activity -> EditableSession -> Model -> Html Msg
viewActivity config language selectedActivity fullSession model =
    let
        checkedIn =
            onlyCheckedIn fullSession

        ( pendingParticipants, completedParticipants ) =
            config.getParticipants checkedIn
                |> EveryDict.partition (\id _ -> config.hasPendingActivity id selectedActivity checkedIn)

        activityDescription =
            div
                [ class "ui unstackable items" ]
                [ div [ class "item" ]
                    [ div [ class "ui image" ]
                        [ span [ class <| "icon-item icon-item-" ++ getActivityIcon (config.wrapActivityType selectedActivity) ] [] ]
                    , div [ class "content" ]
                        [ p [] [ text <| translate language <| Translate.ActivitiesHelp <| config.wrapActivityType selectedActivity ] ]
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
                            config.getName participant

                        imageSrc =
                            config.getAvatarThumb participant

                        imageView =
                            thumbnailImage config.iconClass imageSrc name thumbnailDimensions.height thumbnailDimensions.width
                    in
                        div
                            [ classList
                                [ ( "participant card", True )
                                , ( "active", Just (config.wrapParticipantId participantId) == model.selectedParticipant )
                                ]
                            , onClick <|
                                SetSelectedParticipant <|
                                    Just <|
                                        config.wrapParticipantId participantId
                            ]
                            [ div
                                [ class "image" ]
                                [ imageView ]
                            , div [ class "content" ]
                                [ p [] [ text <| config.getName participant ] ]
                            ]

                participantsCards =
                    if EveryDict.size selectedParticipants == 0 then
                        [ span [] [ text emptySectionMessage ] ]
                    else
                        selectedParticipants
                            |> EveryDict.toList
                            |> List.sortBy (\( _, participant ) -> config.getName participant)
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
                    -- TODO: implement
                    div [] [ text "put measurement form here" ]

                Just (ParticipantMotherId motherId) ->
                    -- TODO: implement
                    div [] [ text "put measurement form here" ]

                Nothing ->
                    emptyNode

        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ text <| translate language <| Translate.ActivitiesTitle <| config.wrapActivityType selectedActivity ]
                , a
                    [ class "link-back"
                    , onClick GoBackToActivitiesPage
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
