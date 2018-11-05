module Pages.Activity.View exposing (view)

import Activity.Utils exposing (getActivityIcon)
import Backend.Session.Model exposing (EditableSession)
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List as List
import Maybe.Extra
import Pages.Activity.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant)
import Translate exposing (Language, translate)
import Utils.Html exposing (tabItem, thumbnailImage)
import ZScore.Model


thumbnailDimensions : { height : Int, width : Int }
thumbnailDimensions =
    { width = 96
    , height = 96
    }


{-| We return the `Maybe id` because we need to indicate which participant we
actually are generating messages for -- that is, which one did we actually
show. It's possible that it would be better to structure this a little
differently, by having the caller determine exactly which participant will
be viewed. However, we "own" part of that data, so that would be a bit awkward.
Another option would be to return the caller's `Html msg` type ... then we
could do our own mapping. The caller would have to pass in a tag for us to
map with, which wouldn't be a problem.
-}
view : Participant id value activity msg -> Language -> NominalDate -> ZScore.Model.Model -> activity -> EditableSession -> Model id -> ( Html (Msg id msg), Maybe id )
view config language currentDate zscores selectedActivity session model =
    let
        participants =
            config.summarizeParticipantsForActivity selectedActivity session

        activityDescription =
            div
                [ class "ui unstackable items" ]
                [ div [ class "item" ]
                    [ div [ class "ui image" ]
                        [ span [ class <| "icon-item icon-item-" ++ getActivityIcon (config.tagActivity selectedActivity) ] [] ]
                    , div [ class "content" ]
                        [ p [] [ text <| translate language <| Translate.ActivitiesHelp <| config.tagActivity selectedActivity ] ]
                    ]
                ]

        tabs =
            let
                pendingTabTitle =
                    EveryDictList.size participants.pending
                        |> Translate.ActivitiesToComplete
                        |> translate language

                completedTabTitle =
                    EveryDictList.size participants.completed
                        |> Translate.ActivitiesCompleted
                        |> translate language
            in
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        -- We compute this so that it's consistent with the tab
        selectedParticipant =
            case model.selectedTab of
                Completed ->
                    model.selectedParticipant
                        |> Maybe.andThen
                            (\participant ->
                                if EveryDictList.member participant participants.completed then
                                    Just participant

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (Maybe.map Tuple.first (EveryDictList.head participants.completed))

                Pending ->
                    model.selectedParticipant
                        |> Maybe.andThen
                            (\participant ->
                                if EveryDictList.member participant participants.pending then
                                    Just participant

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (Maybe.map Tuple.first (EveryDictList.head participants.pending))

        participantsHtml =
            let
                ( selectedParticipants, emptySectionMessage ) =
                    case model.selectedTab of
                        Pending ->
                            ( participants.pending, translate language Translate.NoParticipantsPendingForThisActivity )

                        Completed ->
                            ( participants.completed, translate language Translate.NoParticipantsCompletedForThisActivity )

                viewParticipantCard ( participantId, participant ) =
                    let
                        name =
                            config.getName participant

                        imageSrc =
                            config.getAvatarUrl participant

                        imageView =
                            thumbnailImage (config.iconClass ++ " rounded") imageSrc name thumbnailDimensions.height thumbnailDimensions.width
                    in
                    div
                        [ classList
                            [ ( "participant card", True )
                            , ( "active", Just participantId == selectedParticipant )
                            ]
                        , Just participantId
                            |> SetSelectedParticipant
                            |> onClick
                        ]
                        [ div
                            [ class "image" ]
                            [ imageView ]
                        , div [ class "content" ]
                            [ p [] [ text <| config.getName participant ] ]
                        ]

                participantsCards =
                    if EveryDictList.size selectedParticipants == 0 then
                        [ span [] [ text emptySectionMessage ] ]

                    else
                        selectedParticipants
                            |> EveryDictList.toList
                            |> List.map viewParticipantCard
            in
            div
                [ class "ui participant segment" ]
                [ div [ class "ui four participant cards" ]
                    participantsCards
                ]

        measurementsForm =
            case selectedParticipant of
                Just id ->
                    -- This is a convenience for the way the code was structured ... ideally,
                    -- we'd build a `viewMeasurements` on top of smaller capabilities of the
                    -- `Participant` config, but this is faster for now.
                    config.viewMeasurements language currentDate zscores id selectedActivity session

                Nothing ->
                    emptyNode

        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ config.tagActivity selectedActivity
                        |> Translate.ActivitiesTitle
                        |> translate language
                        |> text
                    ]
                , a
                    [ class "link-back"
                    , onClick GoBackToActivitiesPage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
    ( divKeyed
        [ class "wrap" ]
        [ header |> keyed "header"
        , activityDescription |> keyed "activity-description"
        , tabs |> keyed "tabs"
        , participantsHtml |> keyed "participants"
        , measurementsForm |> keyed "measurements-form"
        ]
    , selectedParticipant
    )
