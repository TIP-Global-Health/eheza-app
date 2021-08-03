module Pages.WellChildParticipant.View exposing (view)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.IndividualEncounterParticipant.Utils exposing (isDailyEncounterActive)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter, WellChildEncounterType(..))
import Date
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMyyyy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildActivity.Utils exposing (generateFutureVaccinationsData, generateVaccinationProgress, getPreviousMeasurements, nextMedicationAdmnistrationData)
import Pages.WellChildEncounter.Utils exposing (..)
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
    div [] <|
        p [ class "label-visit" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterSelectVisit
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
            :: viewWellChildAction language currentDate selectedHealthCenter id isChw db sessions


viewWellChildAction :
    Language
    -> NominalDate
    -> HealthCenterId
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> List (Html App.Model.Msg)
viewWellChildAction language currentDate selectedHealthCenter id isChw db sessions =
    let
        -- Person Well Child participant.
        maybeParticipantId =
            sessions
                |> Dict.toList
                |> List.filter
                    (\( participantId, session ) ->
                        session.encounterType == Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                    )
                |> List.head
                |> Maybe.map Tuple.first

        ( maybeActiveEncounterId, disableAction, completedPediatricCareEncounters ) =
            maybeParticipantId
                |> Maybe.map
                    (\participantId ->
                        Dict.get participantId db.wellChildEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map
                                (\dict ->
                                    let
                                        ( pediatricCareEncounters, newbornEncounters ) =
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
                                          (not <| List.isEmpty pediatricCareEncounters)
                                            -- We will not to allow create new / edit existing action, if
                                            -- we already have one encounter (as there can be only one
                                            --  newborn exam encounter), and it is not active from today.
                                            || (List.head newbornEncounters
                                                    |> Maybe.map (Tuple.second >> isDailyEncounterActive currentDate >> not)
                                                    |> Maybe.withDefault False
                                               )
                                        , []
                                        )

                                    else
                                        let
                                            activeEncounterId =
                                                resolveActiveEncounter pediatricCareEncounters
                                        in
                                        ( activeEncounterId
                                        , -- We will not to allow create new / edit existing action, if
                                          -- there was pediatric care encounter completed today.
                                          List.filter
                                            (\( _, encounter ) ->
                                                encounter.startDate == currentDate && encounter.endDate == Just currentDate
                                            )
                                            pediatricCareEncounters
                                            |> List.isEmpty
                                            |> not
                                        , List.filter
                                            (\( encounterId, _ ) ->
                                                activeEncounterId /= Just encounterId
                                            )
                                            pediatricCareEncounters
                                            |> List.sortBy (Tuple.second >> .encounterType >> encounterToComparable)
                                        )
                                )
                            |> RemoteData.withDefault ( Nothing, False, [] )
                    )
                |> Maybe.withDefault ( Nothing, False, [] )

        action =
            maybeActiveEncounterId
                |> Maybe.map navigateToEncounterAction
                |> Maybe.withDefault
                    (maybeParticipantId
                        |> Maybe.map
                            -- If participant exists, create new encounter for it.
                            (\participantId ->
                                [ Backend.WellChildEncounter.Model.emptyWellChildEncounter participantId currentDate nextEncounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostWellChildEncounter
                                    |> App.Model.MsgIndexedDb
                                    |> onClick
                                ]
                            )
                        -- If participant does not exist, create it.
                        |> Maybe.withDefault
                            [ emptyIndividualEncounterParticipant currentDate id Backend.IndividualEncounterParticipant.Model.WellChildEncounter selectedHealthCenter
                                |> Backend.Model.PostIndividualSession (Backend.IndividualEncounterParticipant.Model.WellChildData nextEncounterType)
                                |> App.Model.MsgIndexedDb
                                |> onClick
                            ]
                    )

        navigateToEncounterAction id_ =
            [ Pages.Page.WellChildEncounterPage id_
                |> UserPage
                |> App.Model.SetActivePage
                |> onClick
            ]

        maybePerson =
            Dict.get id db.people
                |> Maybe.andThen RemoteData.toMaybe

        encounterDueDate =
            Date.max encounterDueDateByMeasurements dueDateForNextEncounter

        encounterDueDateByMeasurements =
            Maybe.Extra.or expectedDateForEncounterByImmunisation expectedDateForEncounterByMedication
                |> Maybe.withDefault currentDate

        ( expectedDateForEncounterByImmunisation, expectedDateForEncounterByMedication ) =
            if isChw || isJust maybeActiveEncounterId then
                ( Nothing, Nothing )

            else
                Maybe.map
                    (\participantId ->
                        let
                            previousMeasurements =
                                generatePreviousMeasurements Nothing participantId db
                                    |> RemoteData.toMaybe
                                    |> Maybe.map getPreviousMeasurements
                                    |> Maybe.withDefault []
                        in
                        ( generateExpectedDateByImmunisation previousMeasurements
                        , generateExpectedDateByMedication previousMeasurements
                        )
                    )
                    maybeParticipantId
                    |> Maybe.withDefault ( Nothing, Nothing )

        generateExpectedDateByImmunisation measurements =
            let
                immunisations =
                    List.filterMap (.immunisation >> getMeasurementValueFunc)
                        measurements

                vaccinationHistories =
                    List.filterMap (.vaccinationHistory >> getMeasurementValueFunc)
                        measurements

                futureVaccinationsData =
                    Maybe.map
                        (\person ->
                            generateVaccinationProgress immunisations vaccinationHistories
                                |> generateFutureVaccinationsData currentDate isChw person
                        )
                        maybePerson
                        |> Maybe.withDefault []
            in
            List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinationsData
                |> List.sortWith Date.compare
                |> List.head

        generateExpectedDateByMedication measurements =
            maybePerson
                |> Maybe.andThen
                    (\person ->
                        person.birthDate
                            |> Maybe.andThen
                                (\birthDate ->
                                    let
                                        ageMonths =
                                            Date.diff Date.Months birthDate currentDate
                                    in
                                    if ageMonths < 15 then
                                        Nothing

                                    else
                                        nextMedicationAdmnistrationData currentDate person measurements
                                            |> Dict.values
                                            |> List.sortWith Date.compare
                                            |> List.reverse
                                            |> List.head
                                )
                    )

        dueDateForNextEncounter =
            Maybe.andThen .birthDate maybePerson
                |> Maybe.map (\birthDate -> dueDateForEncounter birthDate nextEncounterType)
                |> Maybe.withDefault currentDate

        nextEncounterType =
            if isChw then
                NewbornExam

            else
                Maybe.andThen .birthDate maybePerson
                    |> Maybe.map
                        (\birthDate ->
                            if isJust maybeActiveEncounterId || List.isEmpty completedPediatricCareEncounters then
                                -- If there's active encounter, or there were no previous encounters,
                                -- the type is resolved by current date.
                                resolveEncounterTypeOnDate isChw currentDate birthDate

                            else
                                let
                                    encounterTypeByMeasurementsDate =
                                        resolveEncounterTypeOnDate isChw encounterDueDateByMeasurements birthDate

                                    encounterTypeByCompletedEncounters =
                                        completedPediatricCareEncounters
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.map
                                                (Tuple.second
                                                    >> .encounterType
                                                    >> getNextPediatricCareEncounter
                                                )
                                            |> Maybe.withDefault PediatricCareRecurrent
                                in
                                if encounterToComparable encounterTypeByMeasurementsDate < encounterToComparable encounterTypeByCompletedEncounters then
                                    encounterTypeByCompletedEncounters

                                else
                                    encounterTypeByMeasurementsDate
                        )
                    |> Maybe.withDefault PediatricCareRecurrent

        allowNewEncounter =
            not <| Date.compare encounterDueDate currentDate == GT

        encounterLabel =
            (translate language <| Translate.WellChildEncounterType nextEncounterType)
                ++ encounterLabelSuffix

        encounterLabelSuffix =
            if allowNewEncounter then
                ""

            else
                " - " ++ formatDDMMyyyy encounterDueDate

        completedEncountersButtons =
            List.map
                (Tuple.second >> viewCompletedEncounter)
                completedPediatricCareEncounters

        viewCompletedEncounter encounter =
            div [ class "ui primary button disabled" ]
                [ div [ class "button-label" ]
                    [ text <| translate language <| Translate.WellChildEncounterType encounter.encounterType
                    , text " - "
                    , text <| translate language Translate.Completed
                    ]
                , div [ class "icon-back" ] []
                ]

        actionButton =
            div
                (classList
                    [ ( "ui primary button", True )
                    , ( "disabled", disableAction || not allowNewEncounter )
                    ]
                    :: action
                )
                [ div [ class "button-label" ]
                    [ text encounterLabel
                    ]
                , div [ class "icon-back" ] []
                ]

        -- completedPediatricCareEncounters
    in
    completedEncountersButtons ++ [ actionButton ]
