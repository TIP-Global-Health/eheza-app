module Pages.PrenatalParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
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
        -- Person prenatal session.
        maybeSessionId =
            prenatalSessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                            && isPregnancyActive currentDate session
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active prenatal encounter for person. There should not be more than one.
        -- We count the number of completed encounters, so that we know if to
        -- allow 'Pregnancy Outcome' action.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day previous one has ended.
        ( maybeActiveEncounterId, totalCompletedEncounts, encounterWasCompletedToday ) =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        Dict.get sessionId db.prenatalEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (\dict ->
                                    let
                                        activeEncounters =
                                            Dict.toList dict
                                                |> List.filter (\( _, encounter ) -> isNothing encounter.endDate)
                                    in
                                    ( activeEncounters
                                        |> List.head
                                        |> Maybe.map Tuple.first
                                    , Dict.size dict - List.length activeEncounters
                                    , Dict.toList dict
                                        |> List.filter
                                            (\( _, encounter ) ->
                                                encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                            )
                                        |> List.isEmpty
                                        |> not
                                    )
                                )
                            |> RemoteData.withDefault ( Nothing, 0, False )
                    )
                |> Maybe.withDefault ( Nothing, 0, False )

        -- Whether first prenatal encounter for person is in process.
        -- This is True when there's only one encounter, and it's active.
        firstEncounterInProcess =
            maybeSessionId
                |> Maybe.map
                    (\sessionId ->
                        Dict.get sessionId db.prenatalEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (Dict.values
                                    >> (\encounters ->
                                            let
                                                activeEncounters =
                                                    encounters
                                                        |> List.filter (.endDate >> isNothing)
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
                    |> unwrap
                        []
                        navigateToEncounterAction

            else
                maybeSessionId
                    |> Maybe.map
                        -- If prenatal session exists, create new encounter for it.
                        (\sessionId ->
                            [ PrenatalEncounter sessionId currentDate Nothing (Just selectedHealthCenter)
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
                                [ PrenatalEncounter sessionId currentDate Nothing (Just selectedHealthCenter)
                                    |> Backend.Model.PostPrenatalEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        |> Maybe.withDefault []
                    )
                    -- When there's an encounrer, we'll view it.
                    navigateToEncounterAction

        navigateToEncounterAction id_ =
            [ Pages.Page.PrenatalEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]

        navigateToPregnancyOutcomeAction =
            if totalCompletedEncounts > 0 then
                maybeSessionId
                    |> Maybe.map
                        (\sessionId ->
                            [ Pages.Page.PregnancyOutcomePage sessionId
                                |> UserPage
                                |> App.Model.SetActivePage
                                |> onClick
                            ]
                        )
                    |> Maybe.withDefault []

            else
                []

        firstVisitButtonDisabled =
            isJust maybeSessionId && not firstEncounterInProcess

        createFirstEncounterButton =
            if isChw then
                viewButton language
                    []
                    (Translate.PrenatalFirstEncounter Backend.PrenatalEncounter.Model.ChwFirstEncounter)
                    firstVisitButtonDisabled

            else
                viewButton language
                    firstVisitAction
                    (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                    firstVisitButtonDisabled

        createSubsequentEncounterButton =
            viewButton language
                subsequentVisitAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                (not firstVisitButtonDisabled || encounterWasCompletedToday)

        createSecondEncounterButton =
            viewButton language
                []
                (Translate.PrenatalFirstEncounter Backend.PrenatalEncounter.Model.ChwSecondEncounter)
                (not firstVisitButtonDisabled || encounterWasCompletedToday)

        createThirdEncounterButton =
            viewButton language
                []
                (Translate.PrenatalFirstEncounter Backend.PrenatalEncounter.Model.ChwThirdEncounter)
                (not firstVisitButtonDisabled || encounterWasCompletedToday)

        createPostpartumEncounterButton =
            viewButton language
                []
                (Translate.PrenatalFirstEncounter Backend.PrenatalEncounter.Model.ChwPostpartumEncounter)
                (not firstVisitButtonDisabled || encounterWasCompletedToday)

        recordPrenatalOutcomeButton =
            viewButton language
                navigateToPregnancyOutcomeAction
                Translate.RecordPregnancyOutcome
                (List.isEmpty navigateToPregnancyOutcomeAction)
    in
    if isChw then
        div []
            [ p [ class "label-visit" ] [ text <| translate language <| Translate.IndividualEncounterSelectVisit AntenatalEncounter ]
            , createFirstEncounterButton
            , createSecondEncounterButton
            , createThirdEncounterButton
            , createPostpartumEncounterButton
            , div [ class "separator" ] []
            , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]
            , recordPrenatalOutcomeButton
            ]

    else
        div []
            [ p [ class "label-visit" ] [ text <| translate language <| Translate.IndividualEncounterSelectVisit AntenatalEncounter ]
            , createFirstEncounterButton
            , createSubsequentEncounterButton
            , div [ class "separator" ] []
            , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]
            , recordPrenatalOutcomeButton
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
