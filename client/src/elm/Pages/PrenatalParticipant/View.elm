module Pages.PrenatalParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..), emptyPrenatalEncounter)
import Date
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalParticipant.Model exposing (..)
import Pages.PrenatalParticipant.Utils exposing (isPregnancyActive)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw db =
    let
        prenatalSessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant prenatal" ]
        [ viewHeader language id
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewPrenatalActions language currentDate selectedHealthCenter id isChw db) identity prenatalSessions
            ]
        ]


viewHeader : Language -> PersonId -> Html App.Model.Msg
viewHeader language id =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel AntenatalEncounter ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage <| IndividualEncounterParticipantsPage AntenatalEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPrenatalActions : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> ModelIndexedDb -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> Html App.Model.Msg
viewPrenatalActions language currentDate selectedHealthCenter id isChw db prenatalSessions =
    let
        activePrgnancyData =
            prenatalSessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                            && isPregnancyActive currentDate session
                    )
                |> List.head

        maybeSessionId =
            Maybe.map Tuple.first activePrgnancyData

        allEncounters =
            activePrgnancyData
                |> Maybe.map
                    (Tuple.first
                        >> (\sessionId ->
                                Dict.get sessionId db.prenatalEncountersByParticipant
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map
                                        (Dict.toList
                                            >> -- Sort DESC
                                               List.sortWith (\( _, e1 ) ( _, e2 ) -> Date.compare e2.startDate e1.startDate)
                                        )
                                    |> RemoteData.withDefault []
                           )
                    )
                |> Maybe.withDefault []

        completedEncounts =
            List.filter (\( _, encounter ) -> isJust encounter.endDate) allEncounters

        recordPrenatalOutcomeButton =
            viewButton language
                navigateToPregnancyOutcomeAction
                Translate.RecordPregnancyOutcome
                (List.isEmpty navigateToPregnancyOutcomeAction)

        navigateToPregnancyOutcomeAction =
            if List.length completedEncounts > 0 then
                maybeSessionId
                    |> Maybe.map
                        (Pages.Page.PregnancyOutcomePage
                            >> UserPage
                            >> App.Model.SetActivePage
                            >> onClick
                            >> List.singleton
                        )
                    |> Maybe.withDefault []

            else
                []

        label =
            p [ class "label-visit" ] [ text <| translate language <| Translate.IndividualEncounterSelectVisit AntenatalEncounter ]

        encounterTypeSpecificButtons =
            if isChw then
                allEncounters
                    |> List.filter (Tuple.second >> .encounterType >> (==) NurseEncounter)
                    |> viewPrenatalActionsForChw language currentDate selectedHealthCenter id db activePrgnancyData

            else
                allEncounters
                    |> List.filter (Tuple.second >> .encounterType >> (/=) NurseEncounter)
                    |> viewPrenatalActionsForNurse language currentDate selectedHealthCenter id db maybeSessionId

        recordPregannacyOutcomeSection =
            [ div [ class "separator" ] []
            , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]
            , recordPrenatalOutcomeButton
            ]
    in
    (label :: encounterTypeSpecificButtons) ++ recordPregannacyOutcomeSection |> div []


viewPrenatalActionsForNurse :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Maybe IndividualEncounterParticipantId
    -> List ( PrenatalEncounterId, PrenatalEncounter )
    -> List (Html App.Model.Msg)
viewPrenatalActionsForNurse language currentDate selectedHealthCenter id db maybeSessionId encounters =
    let
        activeEncounters =
            encounters
                |> List.filter (\( _, encounter ) -> isNothing encounter.endDate)

        maybeActiveEncounterId =
            activeEncounters
                |> List.head
                |> Maybe.map Tuple.first

        encounterWasCompletedToday =
            encounters
                |> List.filter (\( _, encounter ) -> encounter.startDate == currentDate && encounter.endDate == Just currentDate)
                |> List.isEmpty
                |> not

        -- Whether first prenatal encounter for person is in process.
        -- This is True when there's only one encounter, and it's active.
        firstEncounterInProcess =
            List.length encounters == 1 && List.length activeEncounters == 1

        firstVisitAction =
            -- If first encounter is in process, navigate to it.
            if firstEncounterInProcess then
                maybeActiveEncounterId
                    |> unwrap
                        []
                        navigateToEncounterAction

            else
                maybeSessionId
                    |> Maybe.map
                        -- If prenatal session exists, create new encounter for it.
                        (\sessionId ->
                            [ emptyPrenatalEncounter sessionId currentDate NurseEncounter (Just selectedHealthCenter)
                                |> Backend.Model.PostPrenatalEncounter
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                        )
                    -- If prenatal session does not exist, create it.
                    |> Maybe.withDefault
                        [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.AntenatalEncounter selectedHealthCenter
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
                                [ emptyPrenatalEncounter sessionId currentDate NurseEncounter (Just selectedHealthCenter)
                                    |> Backend.Model.PostPrenatalEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        |> Maybe.withDefault []
                    )
                    -- When there's an encounter, we'll view it.
                    navigateToEncounterAction

        navigateToEncounterAction id_ =
            [ Pages.Page.PrenatalEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]

        firstVisitButtonDisabled =
            isJust maybeSessionId && not firstEncounterInProcess

        createFirstEncounterButton =
            viewButton language
                firstVisitAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                firstVisitButtonDisabled

        createSubsequentEncounterButton =
            viewButton language
                subsequentVisitAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                (not firstVisitButtonDisabled || encounterWasCompletedToday)
    in
    [ createFirstEncounterButton
    , createSubsequentEncounterButton
    ]


viewPrenatalActionsForChw :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Maybe ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
    -> List ( PrenatalEncounterId, PrenatalEncounter )
    -> List (Html App.Model.Msg)
viewPrenatalActionsForChw language currentDate selectedHealthCenter id db activePrgnancyData encounters =
    let
        ( maybeActiveEncounterId, lastEncounterType ) =
            encounters
                |> List.head
                |> Maybe.map
                    (\( encounterId, encounter ) ->
                        let
                            activeEncounterId =
                                if isJust encounter.endDate then
                                    Nothing

                                else
                                    Just encounterId
                        in
                        ( activeEncounterId, Just encounter.encounterType )
                    )
                |> Maybe.withDefault ( Nothing, Nothing )

        -- Button for certain type is active when:
        -- 1. There's an active encounter, and it's type matched button encounter type.
        -- 2. No active encounter, and last completed encounter was of previous type.
        --   For example, second encounter is active, if last completed encounter was first encounter.
        encounterTypeButtonActive encounterType =
            if isJust maybeActiveEncounterId then
                lastEncounterType == Just encounterType

            else
                case encounterType of
                    ChwFirstEncounter ->
                        isNothing lastEncounterType

                    ChwSecondEncounter ->
                        lastEncounterType == Just ChwFirstEncounter

                    ChwThirdEncounter ->
                        lastEncounterType == Just ChwSecondEncounter

                    ChwPostpartumEncounter ->
                        lastEncounterType == Just ChwThirdEncounter

                    -- We shoould never get here, as we deal only with CHW encounters.
                    NurseEncounter ->
                        False

        -- @todo: Use to present warning popup
        pregnancyPastDue =
            activePrgnancyData
                |> Maybe.andThen (Tuple.second >> .eddDate)
                |> Maybe.map
                    (\expected ->
                        Date.compare expected currentDate == LT
                    )
                |> Maybe.withDefault False

        navigateToEncounterAction id_ =
            [ Pages.Page.PrenatalEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]

        createFirstEncounterButton =
            viewButton language
                []
                (Translate.PrenatalEncounterType ChwFirstEncounter)
                (not <| encounterTypeButtonActive ChwFirstEncounter)

        createSecondEncounterButton =
            viewButton language
                []
                (Translate.PrenatalEncounterType ChwSecondEncounter)
                (not <| encounterTypeButtonActive ChwSecondEncounter)

        createThirdEncounterButton =
            viewButton language
                []
                (Translate.PrenatalEncounterType ChwThirdEncounter)
                (not <| encounterTypeButtonActive ChwThirdEncounter)

        createPostpartumEncounterButton =
            viewButton language
                []
                (Translate.PrenatalEncounterType ChwPostpartumEncounter)
                (not <| encounterTypeButtonActive ChwPostpartumEncounter)
    in
    [ createFirstEncounterButton
    , createSecondEncounterButton
    , createThirdEncounterButton
    , createPostpartumEncounterButton
    ]


viewButton : Language -> List (Attribute App.Model.Msg) -> TranslationId -> Bool -> Html App.Model.Msg
viewButton language action lablelTransId disabled =
    let
        attributes =
            [ class "ui primary button"
            , classList [ ( "disabled", disabled ) ]
            ]
                ++ action
    in
    div attributes
        [ div [ class "button-label" ]
            [ text <| translate language lablelTransId ]
        , div [ class "icon-back" ] []
        ]
