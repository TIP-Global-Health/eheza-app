module Pages.Prenatal.Participant.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..), emptyIndividualEncounterParticipant)
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
import Pages.Prenatal.Encounter.Utils exposing (generatePostCreateDestination)
import Pages.Prenatal.Participant.Model exposing (..)
import Pages.Prenatal.Participant.Utils exposing (isPregnancyActive)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> IndividualParticipantInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate selectedHealthCenter id isChw initiator db model =
    let
        prenatalSessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant prenatal" ]
        [ viewHeader language isChw initiator
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewPrenatalActions language currentDate selectedHealthCenter id isChw db model) identity prenatalSessions
            ]
        ]


viewHeader : Language -> Bool -> IndividualParticipantInitiator -> Html Msg
viewHeader language isChw initiator =
    let
        goBackPage =
            case initiator of
                InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage AntenatalEncounter

                InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel AntenatalEncounter isChw ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage goBackPage
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
        ( maybeActiveEncounterId, lastEncounterType, encounterWasCompletedToday ) =
            List.head encounters
                |> Maybe.map
                    (\( encounterId, encounter ) ->
                        let
                            activeEncounterId =
                                if isJust encounter.endDate then
                                    Nothing

                                else
                                    Just encounterId
                        in
                        ( activeEncounterId, Just encounter.encounterType, encounter.endDate == Just currentDate )
                    )
                |> Maybe.withDefault ( Nothing, Nothing, False )

        encounterTypeButtonAction encounterType allowCreateNewSession =
            -- If there's an active encounter, navigate to it.
            Maybe.map navigateToEncounterAction maybeActiveEncounterId
                |> Maybe.withDefault
                    (Maybe.map
                        -- If prenatal session exists, create new encounter for it.
                        (\sessionId ->
                            createNewEncounterMsg currentDate selectedHealthCenter sessionId encounterType DestinationEncounterPage
                        )
                        maybeSessionId
                        |> Maybe.withDefault
                            (if allowCreateNewSession then
                                createNewSessionMsg currentDate selectedHealthCenter id encounterType

                             else
                                []
                            )
                    )

        createFirstEncounterButton =
            viewButton language
                (encounterTypeButtonAction NurseEncounter True)
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                (not firstEncounterButtonEnabled)

        createSubsequentEncounterButton =
            let
                buttonDisabled =
                    firstEncounterButtonEnabled
                        || postpartumEncounterInProcess
                        || encounterWasCompletedToday
            in
            viewButton language
                (encounterTypeButtonAction NurseEncounter False)
                (Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.AntenatalEncounter)
                buttonDisabled

        createPostpartumEncounterButton =
            viewButton language
                (encounterTypeButtonAction NursePostpartumEncounter True)
                (Translate.PrenatalEncounterType NursePostpartumEncounter)
                (not postpartumEncounterButtonEnabled)

        firstEncounterButtonEnabled =
            List.isEmpty encounters
                || firstEncounterInProcess

        postpartumEncounterButtonEnabled =
            Maybe.map
                (\encounterType ->
                    -- Last encounter is not a postpartum encounter,
                    -- and there's no encounter that was completed today.
                    (encounterType /= NursePostpartumEncounter)
                        && not encounterWasCompletedToday
                )
                lastEncounterType
                |> Maybe.withDefault
                    -- When there're no encounters, we allow to
                    -- create postpartum encounter.
                    True

        -- Whether first prenatal encounter for person is in process.
        -- This is True when there's only one encounter, it's active, and
        -- it is of type NurseEncounter.
        firstEncounterInProcess =
            (List.length encounters == 1)
                && isJust maybeActiveEncounterId
                && (Maybe.map ((==) NurseEncounter) lastEncounterType |> Maybe.withDefault False)

        -- Whether postpartum encounter for person is in process.
        -- This is True when current active encounter is of
        -- NursePostpartumEncounter type.
        postpartumEncounterInProcess =
            isJust maybeActiveEncounterId
                && (Maybe.map ((==) NursePostpartumEncounter) lastEncounterType |> Maybe.withDefault False)
    in
    [ createFirstEncounterButton
    , createSubsequentEncounterButton
    , createPostpartumEncounterButton
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

                    -- We should never get here, as we deal only with CHW encounters.
                    NurseEncounter ->
                        False

                    -- We should never get here, as we deal only with CHW encounters.
                    NursePostpartumEncounter ->
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
