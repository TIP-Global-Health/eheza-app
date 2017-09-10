module Pages.Participants.View exposing (view)

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getTotalsNumberPerActivity, participantGotPendingActivity)
import Activity.View exposing (viewActivityTypeFilter)
import App.PageType exposing (Page(..))
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Maybe.Extra exposing (isJust)
import Pages.Participants.Model exposing (Model, Msg(..), Tab(..), thumbnailDimensions)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getParticipantAvatarThumb, getParticipantName, getParticipantTypeAsString)
import Participant.View exposing (viewParticipantTypeFilter)
import Table exposing (..)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)
import Utils.Html exposing (tabItem, thumbnailImage)


view : Language -> Date -> User -> ParticipantsDict -> Model -> List (Html Msg)
view language currentDate currentUser participantsDict model =
    let
        filterMothersByPendingActivity withPending participantId participant =
            case participant.info of
                ParticipantChild child ->
                    False

                ParticipantMother mother ->
                    let
                        motherGotPendingActivity =
                            participantGotPendingActivity currentDate participant

                        children =
                            List.filterMap (\childId -> Dict.get childId participantsDict) mother.children

                        gotPendingActivity =
                            motherGotPendingActivity || List.any (participantGotPendingActivity currentDate) children
                    in
                        if withPending then
                            gotPendingActivity
                        else
                            not <| gotPendingActivity

        mothersWithPendingActivity =
            participantsDict
                |> Dict.filter (filterMothersByPendingActivity True)

        mothersWithoutPendingActivity =
            participantsDict
                |> Dict.filter (filterMothersByPendingActivity False)

        tabs =
            let
                pendingTabTitle =
                    translate language <| Trans.ActivitiesToComplete <| Dict.size mothersWithPendingActivity

                completedTabTitle =
                    translate language <| Trans.ActivitiesCompleted <| Dict.size mothersWithoutPendingActivity
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
                            ( mothersWithPendingActivity, translate language Trans.PendingSectionEmpty )

                        Completed ->
                            ( mothersWithoutPendingActivity, translate language Trans.CompletedSectionEmpty )

                viewMotherCard ( motherId, mother ) =
                    let
                        name =
                            getParticipantName mother

                        imageSrc =
                            getParticipantAvatarThumb mother

                        imageView =
                            if String.isEmpty imageSrc then
                                span
                                    [ class "icon-participant"
                                    , style
                                        [ ( "height", (toString thumbnailDimensions.height) ++ "px" )
                                        , ( "width", (toString thumbnailDimensions.width) ++ "px" )
                                        ]
                                    ]
                                    []
                            else
                                thumbnailImage imageSrc name thumbnailDimensions.height thumbnailDimensions.width
                    in
                        div
                            [ class "card"
                            , onClick <| SetRedirectPage <| App.PageType.Participant motherId
                            ]
                            [ div [ class "image" ]
                                [ imageView ]
                            , div [ class "content" ]
                                [ p [] [ text name ] ]
                            ]

                mothersCards =
                    if Dict.size selectedMothers == 0 then
                        [ span [] [ text emptySectionMessage ] ]
                    else
                        List.map viewMotherCard <|
                            List.sortBy
                                (\( _, mother ) ->
                                    getParticipantName mother
                                )
                            <|
                                Dict.toList selectedMothers
            in
                div [ class "full content" ]
                    [ div [ class "wrap-cards" ]
                        [ div [ class "ui four cards" ]
                            mothersCards
                        ]
                    ]

        endSessionButton =
            div [ class "actions" ]
                [ button
                    [ class "ui fluid button" ]
                    [ text <| translate language Trans.EndSession ]
                ]

        content =
            div
                [ class "ui full segment" ]
                [ mothers, endSessionButton ]
    in
        [ tabs, content ]
