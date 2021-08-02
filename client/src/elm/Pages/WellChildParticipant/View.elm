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
import Pages.WellChildEncounter.Utils exposing (encounterTypeByAge)
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

        ( maybeActiveEncounterId, disableAction ) =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        Dict.get sessionId db.wellChildEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (\dict ->
                                    let
                                        ( pediatricCareEncounetrs, newbornEncounters ) =
                                            Dict.toList dict
                                                |> List.partition (Tuple.second >> .encounterType >> (/=) NewbornExam)

                                        -- Resolve active encounter for person. There should not be more than one.
                                        resolveActiveEncounter encounters =
                                            List.filter (Tuple.second >> isDailyEncounterActive currentDate) encounters
                                                |> List.head
                                                |> Maybe.map Tuple.first
                                    in
                                    if isChw then
                                        ( resolveActiveEncounter newbornEncounters
                                        , -- We will not allow creating newborn exam encounter if
                                          -- child has performed SPV encounter.
                                          (not <| List.isEmpty pediatricCareEncounetrs)
                                            -- We will not to allow create new / edit existing action, if
                                            -- we already have one encounter (as there can be only one
                                            --  newborn exam encounter), and it is not active from today.
                                            || (List.head newbornEncounters
                                                    |> Maybe.map (Tuple.second >> isDailyEncounterActive currentDate >> not)
                                                    |> Maybe.withDefault False
                                               )
                                        )

                                    else
                                        ( resolveActiveEncounter pediatricCareEncounetrs
                                        , -- We will not to allow create new / edit existing action, if
                                          -- there was pediatric care encounter completed today.
                                          List.filter
                                            (\( _, encounter ) ->
                                                encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                            )
                                            pediatricCareEncounetrs
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
                                [ Backend.WellChildEncounter.Model.emptyWellChildEncounter sessionId currentDate newEncounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostWellChildEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If participant does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.WellChildEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualSession (Backend.IndividualEncounterParticipant.Model.WellChildData newEncounterType)
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        newEncounterType =
            if isChw then
                NewbornExam

            else
                Dict.get id db.people
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.andThen .birthDate
                    |> Maybe.map (encounterTypeByAge currentDate isChw)
                    |> Maybe.withDefault PediatricCareRecurrent

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
            , ( "disabled", disableAction )
            ]
            :: action
        )
        [ div [ class "button-label" ]
            [ text <|
                translate language <|
                    Translate.WellChildEncounterType newEncounterType
            ]
        , div [ class "icon-back" ] []
        ]
