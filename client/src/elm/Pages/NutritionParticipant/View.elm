module Pages.NutritionParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NutritionParticipant.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant nutrition" ]
        [ viewHeader language id
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate id db) identity sessions
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
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
            ]
        , a
            [ class "link-back"
            , onClick <|
                App.Model.SetActivePage <|
                    UserPage <|
                        IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.NutritionEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewActions : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html App.Model.Msg
viewActions language currentDate id db sessions =
    let
        -- Person nutrition session.
        maybeSessionId =
            sessions
                |> Dict.toList
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
                        Dict.get sessionId db.nutritionEncountersByParticipant
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

        -- Wither first nutrition encounter for person is in process.
        -- This is True when there's only one encounter, and it's active.
        firstEncounterInProcess =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        Dict.get sessionId db.nutritionEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (Dict.values
                                    >> (\encounters ->
                                            let
                                                activeEncounters =
                                                    encounters
                                                        |> List.filter (isDailyEncounterActive currentDate)
                                            in
                                            List.length encounters == 1 && List.length activeEncounters == 1
                                       )
                                )
                            |> RemoteData.withDefault False
                    )
                |> Maybe.withDefault False

        firstVisitAction =
            -- If first encounter is in process, navigate to it.
            if firstEncounterInProcess then
                maybeActiveEncounterId
                    |> Maybe.map navigateToEncounterAction
                    |> Maybe.withDefault []

            else
                maybeSessionId
                    |> Maybe.map
                        -- If nutrition session exists, create new encounter for it.
                        (\sessionId ->
                            [ Backend.NutritionEncounter.Model.NutritionEncounter sessionId currentDate Nothing
                                |> Backend.Model.PostNutritionEncounter
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                        )
                    -- If nutrition session does not exist, create it.
                    |> Maybe.withDefault
                        [ IndividualEncounterParticipant id
                            Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                            currentDate
                            Nothing
                            Nothing
                            |> Backend.Model.PostIndividualSession
                            |> App.Model.MsgIndexedDb
                            |> onClick
                        ]

        subsequentVisitAction =
            maybeActiveEncounterId
                |> unwrap
                    -- When there's no encounter, we'll create new one.
                    (maybeSessionId
                        |> Maybe.map
                            (\sessionId ->
                                [ Backend.NutritionEncounter.Model.NutritionEncounter sessionId currentDate Nothing
                                    |> Backend.Model.PostNutritionEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        |> Maybe.withDefault []
                    )
                    -- When there's an encounrer, we'll view it.
                    navigateToEncounterAction

        navigateToEncounterAction id_ =
            [ Pages.Page.NutritionEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]

        firstVisitButtonDisabled =
            isJust maybeSessionId && not firstEncounterInProcess
    in
    div []
        [ p [ class "label-antenatal-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
            ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", firstVisitButtonDisabled )
                ]
                :: firstVisitAction
            )
            [ span [ class "text" ]
                [ text <|
                    translate language <|
                        Translate.IndividualEncounterFirstVisit
                            Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                ]
            , span [ class "icon-back" ] []
            ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", not firstVisitButtonDisabled || encounterWasCompletedToday )
                ]
                :: subsequentVisitAction
            )
            [ span [ class "text" ]
                [ text <|
                    translate language <|
                        Translate.IndividualEncounterSubsequentVisit
                            Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                ]
            , span [ class "icon-back" ] []
            ]
        ]
