module Pages.AcuteIllnessParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter, emptyAcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessParticipant.Model exposing (..)
import Pages.AcuteIllnessParticipant.Utils exposing (isAcuteIllnessActive)
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
        [ class "wrap wrap-alt-2 page-participant acute-illness" ]
        [ viewHeader language id
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions2 language currentDate selectedHealthCenter id db) identity sessions
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
                        Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
        , a
            [ class "link-back"
            , onClick <|
                App.Model.SetActivePage <|
                    UserPage <|
                        IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewActions : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html App.Model.Msg
viewActions language currentDate selectedHealthCenter id db sessions =
    let
        maybeSessionId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)
                            && isAcuteIllnessActive currentDate session
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
                        Dict.get sessionId db.acuteIllnessEncountersByParticipant
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
                            -- If session exists, create new encounter for it.
                            (\sessionId ->
                                [ emptyAcuteIllnessEncounter sessionId currentDate (Just selectedHealthCenter)
                                    |> Backend.Model.PostAcuteIllnessEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If session does not exist, create it.
                        |> Maybe.withDefault
                            [ IndividualEncounterParticipant id
                                Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                                currentDate
                                Nothing
                                Nothing
                                (Just selectedHealthCenter)
                                |> Backend.Model.PostIndividualSession
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.AcuteIllnessEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div []
        [ p [ class "label-acute-illness-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
        , button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", encounterWasCompletedToday )
                ]
                :: action
            )
            [ span [ class "text" ]
                [ text <|
                    translate language <|
                        Translate.IndividualEncounterLabel
                            Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                ]
            , span [ class "icon-back" ] []
            ]
        ]


viewActions2 : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html App.Model.Msg
viewActions2 language currentDate selectedHealthCenter id db sessions =
    let
        activeSessions =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)
                            && isAcuteIllnessActive currentDate session
                    )
                |> List.map Tuple.first

        sessionsHtml =
            activeSessions
                |> List.map (viewActiveSession language currentDate selectedHealthCenter id db)

        startIllnessAction =
            IndividualEncounterParticipant id
                Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                currentDate
                Nothing
                Nothing
                (Just selectedHealthCenter)
                |> Backend.Model.PostIndividualSession
                |> App.Model.MsgIndexedDb

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        -- ( maybeActiveEncounterId, encounterWasCompletedToday ) =
        --     maybeSessionId
        --         |> Maybe.map
        --             (\sessionId ->
        --                 Dict.get sessionId db.acuteIllnessEncountersByParticipant
        --                     |> Maybe.withDefault NotAsked
        --                     |> RemoteData.map
        --                         (\dict ->
        --                             ( Dict.toList dict
        --                                 |> List.filter (Tuple.second >> isDailyEncounterActive currentDate)
        --                                 |> List.head
        --                                 |> Maybe.map Tuple.first
        --                             , Dict.toList dict
        --                                 |> List.filter
        --                                     (\( _, encounter ) ->
        --                                         encounter.startDate == currentDate && encounter.endDate == Just currentDate
        --                                     )
        --                                 |> List.isEmpty
        --                                 |> not
        --                             )
        --                         )
        --                     |> RemoteData.withDefault ( Nothing, False )
        --             )
        --         |> Maybe.withDefault ( Nothing, False )
        --
        -- action =
        --     maybeActiveEncounterId
        --         |> Maybe.map navigateToEncounterAction
        --         |> Maybe.withDefault
        --             (maybeSessionId
        --                 |> Maybe.map
        --                     -- If session exists, create new encounter for it.
        --                     (\sessionId ->
        --                         [ emptyAcuteIllnessEncounter sessionId currentDate (Just selectedHealthCenter)
        --                             |> Backend.Model.PostAcuteIllnessEncounter
        --                             |> App.Model.MsgIndexedDb
        --                             |> onClick
        --                         ]
        --                     )
        --                 -- If session does not exist, create it.
        --                 |> Maybe.withDefault
        --                     [ IndividualEncounterParticipant id
        --                         Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
        --                         currentDate
        --                         Nothing
        --                         Nothing
        --                         (Just selectedHealthCenter)
        --                         |> Backend.Model.PostIndividualSession
        --                         |> App.Model.MsgIndexedDb
        --                         |> onClick
        --                     ]
        --             )
        --
        -- navigateToEncounterAction id_ =
        --     [ Pages.Page.AcuteIllnessEncounterPage id_
        --         |> UserPage
        --         |> App.Model.SetActivePage
        --         |> onClick
        --     ]
    in
    div []
        [ div [] sessionsHtml
        , p [ class "label-acute-illness-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
        , button
            [ class "ui primary button"
            , onClick startIllnessAction
            ]
            [ span [ class "text" ]
                [ text <|
                    translate language <|
                        Translate.IndividualEncounterLabel
                            Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                ]
            , span [ class "icon-back" ] []
            ]
        ]


viewActiveSession : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> IndividualEncounterParticipantId -> Html App.Model.Msg
viewActiveSession language currentDate selectedHealthCenter id db sessionId =
    let
        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            Dict.get sessionId db.acuteIllnessEncountersByParticipant
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

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    [ emptyAcuteIllnessEncounter sessionId currentDate (Just selectedHealthCenter)
                        |> Backend.Model.PostAcuteIllnessEncounter
                        |> App.Model.MsgIndexedDb
                        |> onClick
                    ]

        navigateToEncounterAction id_ =
            [ Pages.Page.AcuteIllnessEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div []
        [ button
            (classList
                [ ( "ui primary button", True )
                , ( "disabled", encounterWasCompletedToday )
                ]
                :: action
            )
            [ span [ class "text" ]
                [ text <|
                    translate language <|
                        Translate.IndividualEncounterLabel
                            Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                ]
            , span [ class "icon-back" ] []
            ]
        ]
