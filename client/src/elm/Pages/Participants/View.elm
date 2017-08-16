module Pages.Participants.View exposing (view)

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getPendingNumberPerActivity)
import Activity.View exposing (viewActivityTypeFilter)
import App.PageType exposing (Page(..))
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Pages.Participants.Model exposing (Model, Msg(..))
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getParticipantAvatarThumb, getParticipantName, getParticipantTypeAsString)
import Participant.View exposing (viewParticipantTypeFilter)
import Table exposing (..)
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)


view : Language -> Date -> User -> ParticipantsDict -> Model -> Html Msg
view language currentDate currentUser participants model =
    let
        lowerQuery =
            String.toLower model.query

        acceptableParticipants =
            Dict.filter
                (\participantId participant ->
                    let
                        validName =
                            String.contains lowerQuery (String.toLower <| getParticipantName participant)

                        validType =
                            case model.participantTypeFilter of
                                All ->
                                    True

                                Children ->
                                    case participant.info of
                                        ParticipantChild _ ->
                                            True

                                        _ ->
                                            False

                                Mothers ->
                                    case participant.info of
                                        ParticipantMother _ ->
                                            True

                                        _ ->
                                            False

                        validActivityTypeFilter =
                            List.foldl
                                (\activityType accum ->
                                    if
                                        accum == True
                                        -- We already have found an existing pending activity.
                                    then
                                        True
                                    else
                                        getPendingNumberPerActivity currentDate activityType (Dict.insert participantId participant Dict.empty) > 0
                                )
                                False
                                model.activityTypeFilter
                    in
                        validName && validType && validActivityTypeFilter
                )
                participants
                |> Dict.toList

        searchResult =
            if List.isEmpty acceptableParticipants then
                if Dict.isEmpty participants then
                    -- No participants are present, so it means we are fethcing
                    -- them.
                    div [] []
                else
                    div [ class "ui segment" ] [ text <| translate language Trans.NoParticipantsFound ]
            else
                Table.view config model.tableState acceptableParticipants
    in
        div []
            [ h1 [] [ text <| translate language Trans.Participants ]
            , div [ class "ui input" ]
                [ input
                    [ placeholder <| translate language Trans.SearchByName
                    , onInput SetQuery
                    ]
                    []
                , viewParticipantTypeFilter language SetParticipantTypeFilter model.participantTypeFilter
                ]
            , viewActivityTypeFilterWrapper language model.participantTypeFilter model.activityTypeFilter
            , searchResult
            ]


viewActivityTypeFilterWrapper : Language -> ParticipantTypeFilter -> List ActivityType -> Html Msg
viewActivityTypeFilterWrapper language participantTypeFilter activityTypeFilter =
    let
        childTypeFilters =
            [ div [ class "six wide column" ]
                [ h3 [] [ text <| translate language Trans.Children ]
                , viewActivityTypeFilter SetActivityTypeFilter Children activityTypeFilter
                ]
            ]

        motherTypeFilters =
            [ div [ class "six wide column" ]
                [ h3 [] [ text <| translate language Trans.Mothers ]
                , viewActivityTypeFilter SetActivityTypeFilter Mothers activityTypeFilter
                ]
            ]

        wrapperClasses =
            class "ui grid activity-type-filter"
    in
        case participantTypeFilter of
            All ->
                div [ wrapperClasses ] (childTypeFilters ++ motherTypeFilters)

            Children ->
                div [ wrapperClasses ] childTypeFilters

            Mothers ->
                div [ wrapperClasses ] motherTypeFilters


config : Table.Config ( ParticipantId, Participant ) Msg
config =
    Table.customConfig
        { toId = (\( participantId, _ ) -> toString participantId)
        , toMsg = SetTableState
        , columns =
            [ Table.veryCustomColumn
                { name = "Name"
                , viewData =
                    \( participantId, participant ) ->
                        Table.HtmlDetails []
                            [ a
                                [ href "#"
                                , onClick <| SetRedirectPage <| App.PageType.Participant participantId
                                , class (getParticipantTypeAsString participant)
                                ]
                                [ img [ src <| getParticipantAvatarThumb participant, class "ui avatar image" ] []
                                , text <| getParticipantName participant
                                ]
                            ]
                , sorter = Table.increasingOrDecreasingBy <| Tuple.second >> getParticipantName
                }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "ui celled table", id "participants-table" ] }
        }
