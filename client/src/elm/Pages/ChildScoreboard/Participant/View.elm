module Pages.ChildScoreboard.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardEncounter.Model exposing (ChildScoreboardEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
    exposing
        ( IndividualEncounterParticipant
        , IndividualEncounterType(..)
        , IndividualParticipantInitiator(..)
        , emptyIndividualEncounterParticipant
        )
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.ChildScoreboard.Participant.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant ncd" ]
        [ viewHeader language
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id db) identity sessions
            ]
        ]


viewHeader : Language -> Html App.Model.Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        True
            ]
        , span
            [ class "link-back"
            , onClick <|
                App.Model.SetActivePage <|
                    UserPage <|
                        IndividualEncounterParticipantsPage
                            Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id db sessions =
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        True
            ]
        , viewChildScoreboardAction language currentDate selectedHealthCenter id db sessions
        ]


viewChildScoreboardAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewChildScoreboardAction language currentDate selectedHealthCenter id db sessions =
    let
        -- Person childScoreboard session.
        maybeSessionId =
            Dict.toList sessions
                |> List.filter
                    (\( sessionId, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            Maybe.map
                (\sessionId ->
                    Dict.get sessionId db.childScoreboardEncountersByParticipant
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map
                            (\dict ->
                                ( Dict.toList dict
                                    |> List.filter (Tuple.second >> isDailyEncounterActive currentDate)
                                    |> List.head
                                    |> Maybe.map Tuple.first
                                , Dict.toList dict
                                    |> List.filter
                                        (\( _, encounter ) ->
                                            encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                        )
                                    |> List.isEmpty
                                    |> not
                                )
                            )
                        |> RemoteData.withDefault ( Nothing, False )
                )
                maybeSessionId
                |> Maybe.withDefault ( Nothing, False )

        action =
            Maybe.map navigateToEncounterAction maybeActiveEncounterId
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If childScoreboard session exists, create new encounter for it.
                            (\sessionId ->
                                [ Backend.ChildScoreboardEncounter.Model.emptyChildScoreboardEncounter sessionId currentDate (Just selectedHealthCenter)
                                    |> Backend.Model.PostChildScoreboardEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If childScoreboard session does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualSession Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.ChildScoreboardEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div
        (classList
            [ ( "ui primary button", True )
            , ( "disabled", encounterWasCompletedToday )
            ]
            :: action
        )
        [ div [ class "button-label" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        True
            ]
        , div [ class "icon-back" ] []
        ]
