module Pages.HomeVisitParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (emptyIndividualEncounterParticipant, isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.HomeVisitParticipant.Model exposing (..)
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
        [ class "wrap wrap-alt-2 page-participant home-visit" ]
        [ viewHeader language id
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id db) identity sessions
            ]
        ]


viewHeader : Language -> PersonId -> Html App.Model.Msg
viewHeader language id =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
            ]
        , a
            [ class "link-back"
            , onClick <|
                App.Model.SetActivePage <|
                    UserPage <|
                        IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewActions : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id db sessions =
    let
        -- Person home visit session.
        maybeSessionId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
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
                        Dict.get sessionId db.homeVisitEncountersByParticipant
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
                            -- If home visit session exists, create new encounter for it.
                            (\sessionId ->
                                [ Backend.HomeVisitEncounter.Model.HomeVisitEncounter sessionId currentDate Nothing (Just selectedHealthCenter)
                                    |> Backend.Model.PostHomeVisitEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If home visit session does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualSession
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.HomeVisitEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
            ]
        , div
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
                            Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
                ]
            , div [ class "icon-back" ] []
            ]
        ]
