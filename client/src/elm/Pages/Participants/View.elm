module Pages.Participants.View exposing (view)

import Activity.Utils exposing (getActivityCountForMother)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Backend.Session.Utils exposing (getChildren)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participants.Model exposing (Model, Msg(..), Tab(..))
import Pages.Utils exposing (filterDependentNoResultsMessage, matchMotherAndHerChildren, normalizeFilter, viewNameFilter)
import Translate as Trans exposing (Language, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewModal)
import Utils.Upgrade exposing (force)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 122
    , height = 122
    }


view : Language -> ( SessionId, EditableSession ) -> Model -> Html Msg
view language ( sessionId, session ) model =
    let
        filter =
            normalizeFilter model.filter

        mothersInAttendance =
            force session.checkedIn
                |> .mothers
                |> Dict.filter (matchMotherAndHerChildren filter session.offlineSession)

        summary =
            force session.summaryByParticipant

        ( mothersWithPendingActivity, mothersWithoutPendingActivity ) =
            Dict.partition
                (\motherId mother ->
                    getActivityCountForMother session motherId mother summary
                        |> (\count -> count.pending > 0)
                )
                mothersInAttendance

        tabs =
            let
                pendingTabTitle =
                    Dict.size mothersWithPendingActivity
                        |> Trans.ActivitiesToComplete
                        |> translate language

                completedTabTitle =
                    Dict.size mothersWithoutPendingActivity
                        |> Trans.ActivitiesCompleted
                        |> translate language
            in
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        mothers =
            let
                ( selectedMothers, emptySectionMessage ) =
                    case model.selectedTab of
                        Pending ->
                            ( mothersWithPendingActivity, filterDependentNoResultsMessage language filter Trans.NoParticipantsPending )

                        Completed ->
                            ( mothersWithoutPendingActivity, filterDependentNoResultsMessage language filter Trans.NoParticipantsCompleted )

                viewMotherCard ( motherId, mother ) =
                    div
                        [ class "card"
                        , MotherPage motherId
                            |> SessionPage sessionId
                            |> UserPage
                            |> SetRedirectPage
                            |> onClick
                        ]
                        [ div
                            [ class "image" ]
                            [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
                        , div
                            [ class "content" ]
                          <|
                            p [ class "mother" ] [ text mother.name ]
                                :: (getChildren motherId session.offlineSession
                                        |> List.map (\( _, child ) -> p [ class "child" ] [ text child.name ])
                                   )
                        ]

                mothersCards =
                    if Dict.size selectedMothers == 0 then
                        [ span [] [ text emptySectionMessage ] ]

                    else
                        selectedMothers
                            |> Dict.toList
                            |> List.sortBy (\( _, mother ) -> mother.name)
                            |> List.map viewMotherCard
            in
            div [ class "full content" ]
                [ viewNameFilter language model.filter SetFilter
                , div [ class "wrap-cards" ]
                    [ div [ class "ui four cards" ]
                        mothersCards
                    ]
                ]

        endSessionDialog =
            if model.showEndSessionDialog then
                Just <|
                    div [ class "ui tiny active modal" ]
                        [ div
                            [ class "header" ]
                            [ text <| translate language Trans.AreYouSure ]
                        , div
                            [ class "content" ]
                            [ p []
                                [ text <| translate language Trans.OnceYouEndYourGroupEncounter ]
                            ]
                        , div
                            [ class "actions" ]
                            [ div
                                [ class "two ui buttons" ]
                                [ button
                                    [ class "ui fluid button"
                                    , onClick <| ShowEndSessionDialog False
                                    ]
                                    [ text <| translate language Trans.Cancel ]
                                , button
                                    [ class "ui primary fluid button"
                                    , onClick CloseSession
                                    ]
                                    [ text <| translate language Trans.Continue ]
                                ]
                            ]
                        ]

            else
                Nothing

        endSessionButton =
            div [ class "actions" ]
                [ button
                    [ class "ui fluid primary button"
                    , onClick <| ShowEndSessionDialog True
                    ]
                    [ text <| translate language Trans.EndGroupEncounter ]
                ]

        header =
            div
                [ class "ui basic head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Trans.Participants ]
                , a
                    [ class "link-back"
                    , onClick <| SetRedirectPage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
                    ]
                    [ span [ class "icon-back" ] []
                    , span [] []
                    ]
                , ul [ class "links-head" ]
                    [ li
                        [ onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId AttendancePage ]
                        [ a [] [ span [ class "icon-completed" ] [] ] ]
                    , li
                        [ class "active" ]
                        [ a [] [ span [ class "icon-mother" ] [] ] ]
                    , li
                        [ onClick <| SetRedirectPage <| UserPage <| SessionPage sessionId ActivitiesPage ]
                        [ a [] [ span [ class "icon-measurements" ] [] ] ]
                    ]
                ]

        content =
            div
                [ class "ui full segment" ]
                [ mothers, endSessionButton ]
    in
    div
        [ class "wrap wrap-alt-2 page-participants" ]
        [ header
        , tabs
        , content
        , viewModal endSessionDialog
        ]
