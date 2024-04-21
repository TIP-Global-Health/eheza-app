module Pages.Nutrition.Participant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualParticipantInitiator(..), emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model exposing (NutritionEncounterType(..))
import Backend.NutritionEncounter.Utils exposing (getHomeVisitEncountersForParticipant, getNutritionEncountersForParticipant)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HealthCenterId -> PersonId -> Bool -> IndividualParticipantInitiator -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate selectedHealthCenter id isChw initiator db =
    let
        sessions =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "wrap wrap-alt-2 page-participant individual nutrition" ]
        [ viewHeader language isChw initiator
        , div
            [ class "ui full segment" ]
            [ viewWebData language (viewActions language currentDate selectedHealthCenter id isChw db) identity sessions
            ]
        ]


viewHeader : Language -> Bool -> IndividualParticipantInitiator -> Html App.Model.Msg
viewHeader language isChw initiator =
    let
        goBackPage =
            case initiator of
                InitiatorParticipantsPage ->
                    IndividualEncounterParticipantsPage Backend.IndividualEncounterParticipant.Model.NutritionEncounter

                InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                        isChw
            ]
        , span
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
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
    let
        -- Only CHW are allowed to conduct home visits.
        homeVisitAction =
            if isChw then
                viewHomeVisitAction language currentDate selectedHealthCenter id isChw db sessions

            else
                emptyNode
    in
    div []
        [ p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                        isChw
            ]
        , viewNutritionAction language currentDate selectedHealthCenter id isChw db sessions
        , homeVisitAction
        ]


viewNutritionAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewNutritionAction language currentDate selectedHealthCenter id isChw db sessions =
    let
        -- Person nutrition session.
        maybeSessionId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( _, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            Maybe.map (getNutritionEncountersForParticipant db) maybeSessionId
                |> Maybe.map
                    (\list ->
                        ( List.filter (Tuple.second >> isDailyEncounterActive currentDate) list
                            |> List.head
                            |> Maybe.map Tuple.first
                        , List.filter
                            (\( _, encounter ) ->
                                encounter.startDate == currentDate && encounter.endDate == Just currentDate
                            )
                            list
                            |> List.isEmpty
                            |> not
                        )
                    )
                |> Maybe.withDefault ( Nothing, False )

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If nutrition session exists, create new encounter for it.
                            (\sessionId ->
                                let
                                    encounterType =
                                        if isChw then
                                            NutritionEncounterCHW

                                        else
                                            NutritionEncounterNurse
                                in
                                [ Backend.NutritionEncounter.Model.emptyNutritionEncounter sessionId currentDate encounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostNutritionEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If nutrition session does not exist, create it.
                        |> Maybe.withDefault
                            (let
                                encounterType =
                                    if isChw then
                                        NutritionEncounterCHW

                                    else
                                        NutritionEncounterNurse
                             in
                             [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.NutritionEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualEncounterParticipant (Backend.IndividualEncounterParticipant.Model.NutritionData encounterType)
                                |> App.Model.MsgIndexedDb
                                |> onClick
                             ]
                            )
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.NutritionEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]
    in
    div
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
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                        isChw
            ]
        , div [ class "icon-back" ] []
        ]


viewHomeVisitAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Html App.Model.Msg
viewHomeVisitAction language currentDate selectedHealthCenter id isChw db sessions =
    let
        -- Person Home Visit session.
        maybeSessionId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( _, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        -- Resolve active encounter for person. There should not be more than one.
        -- We also want to know if there's an encounter that was completed today,
        -- (started and ended on the same day), as we do not want to allow creating new encounter
        -- at same day, previous one has ended.
        ( maybeActiveEncounterId, encounterWasCompletedToday ) =
            Maybe.map
                (getHomeVisitEncountersForParticipant db
                    >> (\list ->
                            ( List.filter (Tuple.second >> isDailyEncounterActive currentDate) list
                                |> List.head
                                |> Maybe.map Tuple.first
                            , List.filter
                                (\( _, encounter ) ->
                                    encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                )
                                list
                                |> List.isEmpty
                                |> not
                            )
                       )
                )
                maybeSessionId
                |> Maybe.withDefault ( Nothing, False )

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (maybeSessionId
                        |> Maybe.map
                            -- If home visit session exists, create new encounter for it.
                            (\sessionId ->
                                [ emptyHomeVisitEncounter sessionId currentDate (Just selectedHealthCenter)
                                    |> Backend.Model.PostHomeVisitEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If home visit session does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualEncounterParticipant Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
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
    div
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
                        isChw
            ]
        , div [ class "icon-back" ] []
        ]
