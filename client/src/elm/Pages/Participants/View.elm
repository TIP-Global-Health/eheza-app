module Pages.Participants.View exposing (view)

import Activity.Model exposing (emptySummaryByParticipant)
import Activity.Utils exposing (getActivityCountForMother)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildren)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import LocalData
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participants.Model exposing (Model, Msg(..), Tab(..))
import Pages.Utils exposing (..)
import Translate as Trans exposing (Language, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewModal)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 122
    , height = 122
    }


view : Language -> Bool -> ( SessionId, EditableSession ) -> Model -> Html Msg
view language isChw ( sessionId, session ) model =
    let
        filter =
            normalizeFilter model.filter

        mothersInAttendance =
            session.checkedIn
                |> LocalData.unwrap
                    Dict.empty
                    (.mothers >> Dict.filter (matchMotherAndHerChildren filter session.offlineSession))

        summary =
            session.summaryByParticipant |> LocalData.withDefault emptySummaryByParticipant

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
                    viewConfirmationDialog language
                        Trans.AreYouSure
                        Trans.OnceYouEndYourGroupEncounter
                        CloseSession
                        (ShowEndSessionDialog False)

            else
                Nothing

        goBackPage =
            if isChw then
                UserPage GroupEncounterTypesPage

            else
                UserPage ClinicsPage

        endSessionButton =
            div [ class "actions" ]
                [ button
                    [ class "ui fluid button green"
                    , onClick <| SetRedirectPage <| UserPage ClinicalPage
                    ]
                    [ text <| translate language Trans.EndGroupEncounter ]
                ]

        header =
            div
                [ class "ui basic head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Trans.Participants ]
                , span
                    [ class "link-back"
                    , onClick <| SetRedirectPage goBackPage
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
