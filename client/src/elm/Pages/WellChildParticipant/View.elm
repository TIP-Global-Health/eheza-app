module Pages.WellChildParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter, WellChildEncounterType(..))
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildParticipant.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant well-child" ]
        [ viewHeader language id isChw
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id isChw db) identity sessions
            ]
        ]


viewHeader : Language -> PersonId -> Bool -> Html App.Model.Msg
viewHeader language id isChw =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , a
            [ class "link-back"
            , onClick <|
                App.Model.SetActivePage <|
                    UserPage <|
                        IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.WellChildEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id isChw db sessions =
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , viewWellChildAction language currentDate selectedHealthCenter id isChw db sessions
        ]


viewWellChildAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewWellChildAction language currentDate selectedHealthCenter id isChw db sessions =
    let
        -- Person Well Child participant.
        maybeSessionId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        Dict.get sessionId db.wellChildEncountersByParticipant
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
                |> Maybe.withDefault ( Nothing, False )

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If participant exists, create new encounter for it.
                            (\sessionId ->
                                [ Backend.WellChildEncounter.Model.emptyWellChildEncounter sessionId currentDate encounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostWellChildEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If participant does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.WellChildEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualSession (Backend.IndividualEncounterParticipant.Model.WellChildData encounterType)
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        encounterType =
            if isChw then
                NewbornExam

            else
                PediatricCare

        navigateToEncounterAction id_ =
            [ Pages.Page.WellChildEncounterPage id_
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
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , div [ class "icon-back" ] []
        ]
