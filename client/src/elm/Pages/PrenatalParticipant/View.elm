module Pages.PrenatalParticipant.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortEncounterTuplesDesc)
import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounter
        , PrenatalEncounterPostCreateDestination(..)
        , PrenatalEncounterType(..)
        , RecordPreganancyInitiator(..)
        , emptyPrenatalEncounter
        )
import Date
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.Utils exposing (generatePostCreateDestination)
import Pages.PrenatalParticipant.Model exposing (..)
import Pages.PrenatalParticipant.Utils exposing (isPregnancyActive)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate selectedHealthCenter id isChw db model =
    let
        prenatalSessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant prenatal" ]
        [ viewHeader language id isChw
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewPrenatalActions language currentDate selectedHealthCenter id isChw db model) identity prenatalSessions
            ]
        ]


viewHeader : Language -> PersonId -> Bool -> Html Msg
viewHeader language id isChw =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel AntenatalEncounter isChw ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| IndividualEncounterParticipantsPage AntenatalEncounter
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPrenatalActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html Msg
viewPrenatalActions language currentDate selectedHealthCenter id isChw db model prenatalSessions =
    let
        activePregnancyData =
            prenatalSessions
                |> Dict.toList
                |> List.filter
                    (\( sessionId, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                            && isPregnancyActive currentDate session
                    )
                |> List.head

        maybeSessionId =
            Maybe.map Tuple.first activePregnancyData

        allEncounters =
            activePregnancyData
                |> Maybe.map
                    (Tuple.first
                        >> (\sessionId ->
                                Dict.get sessionId db.prenatalEncountersByParticipant
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map
                                        (Dict.toList
                                            >> -- Sort DESC
                                               List.sortWith sortEncounterTuplesDesc
                                        )
                                    |> RemoteData.withDefault []
                           )
                    )
                |> Maybe.withDefault []

        ( nurseEncounters, chwEncounters ) =
            List.partition (Tuple.second >> .encounterType >> (==) NurseEncounter) allEncounters

        completedEncounts =
            List.filter (\( _, encounter ) -> isJust encounter.endDate) allEncounters

        recordPrenatalOutcomeButton =
            viewButton language
                navigateToPregnancyOutcomeActionForButton
                Translate.RecordPregnancyOutcome
                (List.isEmpty navigateToPregnancyOutcomeActionForButton)

        navigateToPregnancyOutcomeActionForButton =
            navigateToPregnancyOutcomeAction InitiatorParticipantPage

        navigateToPregnancyOutcomeAction destinationPage =
            if List.length completedEncounts > 0 then
                maybeSessionId
                    |> Maybe.map
                        (Pages.Page.PregnancyOutcomePage destinationPage
                            >> UserPage
                            >> SetActivePage
                            >> onClick
                            >> List.singleton
                        )
                    |> Maybe.withDefault []

            else
                []

        label =
            p [ class "label-visit" ] [ text <| translate language <| Translate.IndividualEncounterSelectVisit AntenatalEncounter isChw ]

        hasNurseEncounter =
            not <| List.isEmpty nurseEncounters

        encounterTypeSpecificButtons =
            if isChw then
                viewPrenatalActionsForChw language currentDate selectedHealthCenter id db activePregnancyData chwEncounters hasNurseEncounter

            else
                viewPrenatalActionsForNurse language currentDate selectedHealthCenter id db maybeSessionId nurseEncounters

        recordPregnancyOutcomeSection =
            [ div [ class "separator" ] []
            , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]
            , recordPrenatalOutcomeButton
            ]

        showWarningPopup =
            isChw && model.showWarningPopup && isJust maybeSessionId && List.isEmpty chwEncounters

        popup =
            if showWarningPopup then
                viewModal <|
                    warningPopup language
                        currentDate
                        (navigateToPregnancyOutcomeAction InitiatorWarningPopup)

            else
                emptyNode
    in
    [ label, popup ]
        ++ encounterTypeSpecificButtons
        ++ recordPregnancyOutcomeSection
        |> div []


warningPopup : Language -> NominalDate -> List (Attribute Msg) -> Maybe (Html Msg)
warningPopup language currentDate navigateToPregnancyOutcomeAction =
    Just <|
        div [ class "ui active modal open-pregnancy-popup" ]
            [ div [ class "content" ] <|
                [ div [ class "popup-heading-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                    ]
                , div [ class "popup-action" ] [ text <| translate language Translate.LabelOnePregnancyEpisodeOpen ++ "." ]
                , div [ class "popup-action" ] [ text <| translate language Translate.LabelSeenHealthcareProviderForPregnancy ++ "?" ]
                ]
            , div
                [ class "actions" ]
                [ div [ class "two ui buttons" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick CloseWarningPopup
                        ]
                        [ text <| translate language Translate.Yes ]
                    , button
                        (class "ui primary fluid button" :: navigateToPregnancyOutcomeAction)
                        [ text <| translate language Translate.LabelDocumentPregnancyOutcome ]
                    ]
                ]
            ]


viewPrenatalActionsForNurse :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Maybe IndividualEncounterParticipantId
    -> List ( PrenatalEncounterId, PrenatalEncounter )
    -> List (Html Msg)
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
                    |> Maybe.map navigateToEncounterAction
                    |> Maybe.withDefault
                        []

            else
                maybeSessionId
                    |> Maybe.map
                        -- If prenatal session exists, create new encounter for it.
                        (\sessionId ->
                            createNewEncounterMsg currentDate selectedHealthCenter sessionId NurseEncounter DestinationEncounterPage
                        )
                    -- If prenatal session does not exist, create it.
                    |> Maybe.withDefault
                        (createNewSessionMsg currentDate selectedHealthCenter id NurseEncounter)

        subsequentVisitAction =
            maybeActiveEncounterId
                -- When there's an encounter, we'll view it.
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    -- When there's no encounter, we'll create new one.
                    (maybeSessionId
                        |> Maybe.map
                            (\sessionId ->
                                createNewEncounterMsg currentDate selectedHealthCenter sessionId NurseEncounter DestinationEncounterPage
                            )
                        |> Maybe.withDefault []
                    )

        firstVisitButtonDisabled =
            isJust maybeSessionId
                && not (List.isEmpty encounters)
                && not firstEncounterInProcess

        createFirstEncounterButton =
            viewButton language
                firstVisitAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                firstVisitButtonDisabled

        createSubsequentEncounterButton =
            viewButton language
                subsequentVisitAction
                (Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
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
    -> Bool
    -> List (Html Msg)
viewPrenatalActionsForChw language currentDate selectedHealthCenter id db activePregnancyData encounters hasNurseEncounter =
    let
        maybeSessionId =
            Maybe.map Tuple.first activePregnancyData

        ( maybeActiveEncounterId, lastEncounterType, encounterWasCompletedToday ) =
            encounters
                |> List.head
                |> Maybe.map
                    (\( encounterId, encounter ) ->
                        let
                            activeEncounterId =
                                if isJust encounter.endDate || encounter.startDate /= currentDate then
                                    Nothing

                                else
                                    Just encounterId
                        in
                        ( activeEncounterId, Just encounter.encounterType, encounter.endDate == Just currentDate )
                    )
                |> Maybe.withDefault ( Nothing, Nothing, False )

        -- Button for certain type is active when:
        -- 1. There's an active encounter, and it's type matches button encounter type.
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
                        lastEncounterType == Just ChwFirstEncounter && not encounterWasCompletedToday

                    ChwThirdPlusEncounter ->
                        -- There can be multiple 'ChwThird' encounters.
                        (lastEncounterType == Just ChwSecondEncounter || lastEncounterType == Just ChwThirdPlusEncounter)
                            && not encounterWasCompletedToday

                    ChwPostpartumEncounter ->
                        case lastEncounterType of
                            Just lastEncounterType_ ->
                                -- Last encounter is not a postpartum encounter,
                                -- and there's no encounter that was completed today.
                                (lastEncounterType_ /= ChwPostpartumEncounter)
                                    && not encounterWasCompletedToday

                            Nothing ->
                                -- When there're no encounters, we allow to
                                -- create postpartum encounter.
                                True

                    -- We shoould never get here, as we deal only with CHW encounters.
                    NurseEncounter ->
                        False

        encounterTypeButtonAction encounterType =
            maybeActiveEncounterId
                -- If there's an active encounter, navigate to it.
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If prenatal session exists, create new encounter for it.
                            (\sessionId ->
                                let
                                    postCreateDestination =
                                        generatePostCreateDestination encounterType hasNurseEncounter
                                in
                                createNewEncounterMsg currentDate selectedHealthCenter sessionId encounterType postCreateDestination
                            )
                        |> Maybe.withDefault
                            -- Prenatal session does not exist. Create it, if it's a first or postpartum encounter.
                            (if encounterType == ChwFirstEncounter || encounterType == ChwPostpartumEncounter then
                                createNewSessionMsg currentDate selectedHealthCenter id encounterType

                             else
                                -- There must be a session, as this is not the first encounter.
                                []
                            )
                    )

        createFirstEncounterButton =
            viewButton language
                (encounterTypeButtonAction ChwFirstEncounter)
                (Translate.PrenatalEncounterType ChwFirstEncounter)
                (not <| encounterTypeButtonActive ChwFirstEncounter)

        createSubsequentEncounterButton =
            viewButton language
                (encounterTypeButtonAction subsequentEncounterType)
                (Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                (not <| encounterTypeButtonActive subsequentEncounterType)

        -- Used for subsequent encounter button, which is responsible
        -- to start / navigate to second or third encounters.
        subsequentEncounterType =
            case lastEncounterType of
                Just ChwFirstEncounter ->
                    ChwSecondEncounter

                Just ChwSecondEncounter ->
                    if isJust maybeActiveEncounterId then
                        ChwSecondEncounter

                    else
                        ChwThirdPlusEncounter

                _ ->
                    ChwThirdPlusEncounter

        createPostpartumEncounterButton =
            viewButton language
                (encounterTypeButtonAction ChwPostpartumEncounter)
                (Translate.PrenatalEncounterType ChwPostpartumEncounter)
                (not <| encounterTypeButtonActive ChwPostpartumEncounter)
    in
    [ createFirstEncounterButton
    , createSubsequentEncounterButton
    , createPostpartumEncounterButton
    ]


createNewSessionMsg : NominalDate -> HealthCenterId -> PersonId -> PrenatalEncounterType -> List (Attribute Msg)
createNewSessionMsg currentDate selectedHealthCenter personId encounterType =
    emptyIndividualEncounterParticipant currentDate personId Backend.IndividualEncounterParticipant.Model.AntenatalEncounter selectedHealthCenter
        |> Backend.Model.PostIndividualSession (Backend.IndividualEncounterParticipant.Model.AntenatalData encounterType)
        |> MsgBackend
        |> onClick
        |> List.singleton


createNewEncounterMsg : NominalDate -> HealthCenterId -> IndividualEncounterParticipantId -> PrenatalEncounterType -> PrenatalEncounterPostCreateDestination -> List (Attribute Msg)
createNewEncounterMsg currentDate selectedHealthCenter sessionId encounterType postCreateDestination =
    emptyPrenatalEncounter sessionId currentDate encounterType (Just selectedHealthCenter)
        |> Backend.Model.PostPrenatalEncounter postCreateDestination
        |> MsgBackend
        |> onClick
        |> List.singleton


navigateToEncounterAction : PrenatalEncounterId -> List (Attribute Msg)
navigateToEncounterAction encounterId =
    Pages.Page.PrenatalEncounterPage encounterId
        |> UserPage
        |> SetActivePage
        |> onClick
        |> List.singleton


viewButton : Language -> List (Attribute Msg) -> TranslationId -> Bool -> Html Msg
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
