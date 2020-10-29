module Pages.AcuteIllnessParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter, emptyAcuteIllnessEncounter)
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


view : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Html Msg
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
            [ viewWebData language (viewContent language currentDate selectedHealthCenter id db) identity sessions
            ]
        ]


viewHeader : Language -> PersonId -> Html Msg
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
                SetActivePage <|
                    UserPage <|
                        IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html Msg
viewContent language currentDate selectedHealthCenter id db sessions =
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

        activeIllnesses =
            activeSessions
                |> List.filterMap (viewActiveIllness language currentDate selectedHealthCenter id db)

        startIllnessAction =
            IndividualEncounterParticipant id
                Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                currentDate
                Nothing
                Nothing
                (Just selectedHealthCenter)
                |> Backend.Model.PostIndividualSession
                |> MsgBackend

        viewLabel cssClass transId =
            p [ class cssClass ] [ text <| translate language transId ]

        recordNewIllnessButton =
            viewButton startIllnessAction (Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter)

        recordIllnessOutcomeButton =
            viewButton RecordIllnessOutcome Translate.RecordAcuteIllnessOutcome

        viewButton action lablelTransId =
            div
                [ class "ui primary button"
                , onClick action
                ]
                [ div [ class "button-label" ]
                    [ text <| translate language lablelTransId ]
                , div [ class "icon-back" ] []
                ]
    in
    if List.isEmpty activeIllnesses then
        div []
            [ viewLabel "select-visit" <| Translate.IndividualEncounterSelectVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            , recordNewIllnessButton
            ]

    else
        div []
            [ viewLabel "select-illness" Translate.SelectExistingAcuteIllness
            , div [ class "active-illnesses" ] activeIllnesses
            , viewLabel "start-new-encounter" Translate.StrartNewAcuteIllnessHelper
            , recordNewIllnessButton
            , div [ class "separator" ] []
            , viewLabel "" Translate.CloseAcuteIllnessLabel
            , recordIllnessOutcomeButton
            ]


viewActiveIllness : Language -> NominalDate -> HealthCenterId -> PersonId -> ModelIndexedDb -> IndividualEncounterParticipantId -> Maybe (Html Msg)
viewActiveIllness language currentDate selectedHealthCenter id db sessionId =
    let
        sessionEncounters =
            Dict.get sessionId db.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map Dict.toList

        mDiagnosis =
            sessionEncounters
                |> Maybe.andThen
                    (List.map Tuple.second
                        >> List.sortWith (\e1 e2 -> Gizra.NominalDate.compare e1.startDate e2.startDate)
                        >> List.head
                        >> Maybe.map .diagnosis
                    )
    in
    case mDiagnosis of
        Nothing ->
            Nothing

        Just NoAcuteIllnessDiagnosis ->
            Nothing

        Just diagnosis ->
            sessionEncounters
                |> Maybe.andThen
                    (\encounters ->
                        let
                            maybeActiveEncounterId =
                                List.filter (Tuple.second >> isDailyEncounterActive currentDate) encounters
                                    |> List.head
                                    |> Maybe.map Tuple.first

                            encounterWasCompletedToday =
                                encounters
                                    |> List.filter
                                        (Tuple.second
                                            >> (\encounter -> encounter.startDate == currentDate && encounter.endDate == Just currentDate)
                                        )
                                    |> List.isEmpty
                                    |> not

                            action =
                                maybeActiveEncounterId
                                    |> Maybe.map navigateToEncounterAction
                                    |> Maybe.withDefault
                                        (emptyAcuteIllnessEncounter sessionId currentDate (Just selectedHealthCenter)
                                            |> Backend.Model.PostAcuteIllnessEncounter
                                            |> MsgBackend
                                        )

                            navigateToEncounterAction id_ =
                                Pages.Page.AcuteIllnessEncounterPage id_
                                    |> UserPage
                                    |> SetActivePage

                            encounterLabel =
                                if List.length encounters == 1 && isJust maybeActiveEncounterId then
                                    Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter

                                else
                                    Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                        in
                        Just <|
                            div
                                [ classList
                                    [ ( "ui primary button active-illness", True )
                                    , ( "disabled", encounterWasCompletedToday )
                                    ]
                                , onClick action
                                ]
                                [ div [ class "button-label" ]
                                    [ div [ class "encounter-label" ]
                                        [ text <| translate language encounterLabel ]
                                    , div [] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
                                    ]
                                , div [ class "icon-back" ] []
                                ]
                    )
