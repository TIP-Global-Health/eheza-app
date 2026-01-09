module Pages.HealthyStart.Participant.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HealthyStartEncounter.Model
    exposing
        ( HealthyStartEncounter
        , HealthyStartEncounterType(..)
        , RecordPreganancyInitiator(..)
        , emptyHealthyStartEncounter
        )
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..), emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getHealthyStartEncountersForParticipant)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.HealthyStart.Participant.Model exposing (..)
import Pages.HealthyStart.Participant.Utils exposing (isPregnancyActive)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (sortEncounterTuplesDesc)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> IndividualParticipantInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate selectedHealthCenter id initiator db model =
    let
        prenatalSessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant individual prenatal" ]
        [ viewHeader language initiator
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id db model) identity prenatalSessions
            ]
        ]


viewHeader : Language -> IndividualParticipantInitiator -> Html Msg
viewHeader language initiator =
    let
        goBackPage =
            case initiator of
                InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter

                InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter False ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewActions :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Model
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html Msg
viewActions language currentDate selectedHealthCenter id db model prenatalSessions =
    let
        activePregnancyData =
            prenatalSessions
                |> Dict.toList
                |> List.filter
                    (\( _, session ) ->
                        (session.encounterType == Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter)
                            && isPregnancyActive currentDate session
                    )
                |> List.head

        maybeSessionId =
            Maybe.map Tuple.first activePregnancyData

        allEncounters =
            activePregnancyData
                |> Maybe.map
                    (Tuple.first
                        >> (getHealthyStartEncountersForParticipant db
                                >> -- Sort DESC
                                   List.sortWith sortEncounterTuplesDesc
                           )
                    )
                |> Maybe.withDefault []

        ( nurseEncounters, chwEncounters ) =
            List.partition
                (\( _, encouter ) ->
                    List.member encouter.encounterType [ NurseEncounter, NursePostpartumEncounter ]
                )
                allEncounters

        completedEncounts =
            List.filter (\( _, encounter ) -> isJust encounter.endDate) allEncounters

        -- @todo
        -- recordHealthyStartOutcomeButton =
        --     viewButton language
        --         navigateToPregnancyOutcomeActionForButton
        --         Translate.RecordPregnancyOutcome
        --         (List.isEmpty navigateToPregnancyOutcomeActionForButton)
        --
        -- navigateToPregnancyOutcomeActionForButton =
        --     navigateToPregnancyOutcomeAction InitiatorParticipantPage
        --
        -- navigateToPregnancyOutcomeAction destinationPage =
        --     if List.length completedEncounts > 0 then
        --         maybeSessionId
        --             |> Maybe.map
        --                 (Pages.Page.PregnancyOutcomePage destinationPage
        --                     >> UserPage
        --                     >> SetActivePage
        --                     >> onClick
        --                     >> List.singleton
        --                 )
        --             |> Maybe.withDefault []
        --
        --     else
        --         []
        label =
            p [ class "label-visit" ]
                [ text <|
                    translate language <|
                        Translate.IndividualEncounterSelectVisit
                            Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter
                            False
                ]

        encounterTypeSpecificButtons =
            viewActionsForNurse language currentDate selectedHealthCenter id db maybeSessionId nurseEncounters

        recordPregnancyOutcomeSection =
            [ div [ class "separator" ] []
            , p [ class "label-pregnancy-concluded" ] [ text <| translate language Translate.PregnancyConcludedLabel ]

            -- @todo
            -- , recordHealthyStartOutcomeButton
            ]
    in
    label
        :: encounterTypeSpecificButtons
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


viewActionsForNurse :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> ModelIndexedDb
    -> Maybe IndividualEncounterParticipantId
    -> List ( HealthyStartEncounterId, HealthyStartEncounter )
    -> List (Html Msg)
viewActionsForNurse language currentDate selectedHealthCenter id db maybeSessionId encounters =
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

        firstEncounterButton =
            viewButton language
                firstEnconterButtonAction
                (Translate.IndividualEncounterFirstVisit Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter)
                (not firstEncounterButtonEnabled)

        subsequentEncounterButton =
            let
                subsequentEnconterButtonAction =
                    Maybe.map navigateToEncounterAction maybeActiveEncounterId
                        |> Maybe.withDefault
                            (Maybe.map
                                -- If prenatal session exists, create new encounter for it.
                                (\sessionId ->
                                    createNewEncounterMsg currentDate selectedHealthCenter sessionId NurseEncounter
                                )
                                maybeSessionId
                                |> Maybe.withDefault []
                            )

                buttonDisabled =
                    firstEncounterButtonEnabled
                        || postpartumEncounterInProcess
                        || encounterWasCompletedToday
            in
            viewButton language
                subsequentEnconterButtonAction
                (Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter)
                buttonDisabled

        postpartumEncounterButton =
            viewButton language
                postpartumEncounterButtonAction
                (Translate.HealthyStartEncounterType NursePostpartumEncounter)
                (not postpartumEncounterButtonEnabled)

        firstEncounterButtonEnabled =
            List.isEmpty encounters
                || firstEncounterInProcess

        firstEnconterButtonAction =
            Maybe.map navigateToEncounterAction maybeActiveEncounterId
                |> Maybe.withDefault
                    (Maybe.map
                        -- If prenatal session exists, create new encounter for it.
                        (\sessionId ->
                            createNewEncounterMsg currentDate selectedHealthCenter sessionId NurseEncounter
                        )
                        maybeSessionId
                        |> Maybe.withDefault (createNewSessionMsg currentDate selectedHealthCenter id NurseEncounter)
                    )

        postpartumEncounterButtonEnabled =
            postpartumEncounterInProcess
                || (Maybe.map
                        (\encounterType ->
                            -- Last encounter is not a postpartum encounter,
                            -- and there's no encounter that was completed today.
                            (encounterType /= NursePostpartumEncounter)
                                && not encounterWasCompletedToday
                        )
                        lastEncounterType
                        |> Maybe.withDefault
                            -- When there're no encounters, we allow to
                            --  postpartum encounter.
                            True
                   )

        postpartumEncounterButtonAction =
            if postpartumEncounterInProcess then
                Maybe.map navigateToEncounterAction maybeActiveEncounterId
                    |> Maybe.withDefault []

            else
                Maybe.map
                    -- If prenatal session exists, create new encounter for it.
                    (\sessionId ->
                        createNewEncounterMsg currentDate selectedHealthCenter sessionId NursePostpartumEncounter
                    )
                    maybeSessionId
                    |> Maybe.withDefault (createNewSessionMsg currentDate selectedHealthCenter id NursePostpartumEncounter)

        -- Whether the first prenatal encounter for the person is in process.
        -- This is True when there's only one encounter, it's active, and
        -- it is of type NurseEncounter.
        firstEncounterInProcess =
            (List.length encounters == 1)
                && isJust maybeActiveEncounterId
                && (Maybe.map ((==) NurseEncounter) lastEncounterType |> Maybe.withDefault False)

        -- Whether postpartum encounter for the person is in process.
        -- This is True when the current active encounter is of
        -- NursePostpartumEncounter type.
        postpartumEncounterInProcess =
            isJust maybeActiveEncounterId
                && (Maybe.map ((==) NursePostpartumEncounter) lastEncounterType |> Maybe.withDefault False)
    in
    [ firstEncounterButton
    , subsequentEncounterButton
    , postpartumEncounterButton
    ]


createNewSessionMsg : NominalDate -> HealthCenterId -> PersonId -> HealthyStartEncounterType -> List (Attribute Msg)
createNewSessionMsg currentDate selectedHealthCenter personId encounterType =
    emptyIndividualEncounterParticipant currentDate personId Backend.IndividualEncounterParticipant.Model.HealthyStartEncounter selectedHealthCenter
        |> Backend.Model.PostIndividualEncounterParticipant (Backend.IndividualEncounterParticipant.Model.HealthyStartData encounterType)
        |> MsgBackend
        |> onClick
        |> List.singleton


createNewEncounterMsg : NominalDate -> HealthCenterId -> IndividualEncounterParticipantId -> HealthyStartEncounterType -> List (Attribute Msg)
createNewEncounterMsg currentDate selectedHealthCenter sessionId encounterType =
    emptyHealthyStartEncounter sessionId currentDate encounterType (Just selectedHealthCenter)
        |> Backend.Model.PostHealthyStartEncounter
        |> MsgBackend
        |> onClick
        |> List.singleton


navigateToEncounterAction : HealthyStartEncounterId -> List (Attribute Msg)
navigateToEncounterAction encounterId =
    Pages.Page.HealthyStartEncounterPage encounterId
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
