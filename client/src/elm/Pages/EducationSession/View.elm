module Pages.EducationSession.View exposing (view)

import AssocList as Dict
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isPersonAnAdult)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.EducationSession.Model exposing (InitialResultsDisplay(..), Model, Msg(..), ViewMode(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (matchFilter, normalizeFilter, viewCheckBoxMultipleSelectInput, viewEncounterActionButton, viewQuestionLabel)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Maybe VillageId -> EducationSessionId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate mVillageId id db model =
    let
        session =
            Dict.get id db.educationSessions
                |> Maybe.withDefault NotAsked

        villageId =
            Maybe.map Success mVillageId
                |> Maybe.withDefault NotAsked
    in
    RemoteData.append villageId session
        |> viewWebData language (viewHeaderAndContent language currentDate id db model) identity


viewHeaderAndContent : Language -> NominalDate -> EducationSessionId -> ModelIndexedDb -> Model -> ( VillageId, EducationSession ) -> Html Msg
viewHeaderAndContent language currentDate id db model ( villageId, session ) =
    let
        header =
            viewHeader language viewMode session

        content =
            case viewMode of
                ModeTopics topics ->
                    viewTopicsContent language currentDate id session topics

                ModeAttendance participants ->
                    viewParticipantsContent language currentDate villageId id session db model participants

        viewMode =
            Maybe.withDefault
                (if EverySet.isEmpty session.topics then
                    ModeTopics session.topics

                 else
                    ModeAttendance session.participants
                )
                model.viewMode
    in
    div [ class "page-activity education-session" ]
        [ header
        , content
        ]


viewHeader : Language -> ViewMode -> EducationSession -> Html Msg
viewHeader language viewMode session =
    let
        ( label, goBackAction ) =
            case viewMode of
                ModeTopics _ ->
                    ( Translate.HealthTopics
                    , SetActivePage <| UserPage GroupEncounterTypesPage
                    )

                ModeAttendance _ ->
                    ( Translate.Attendance
                    , SetViewMode <| ModeTopics session.topics
                    )
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language label ]
        , span
            [ class "link-back"
            , onClick goBackAction
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewTopicsContent : Language -> NominalDate -> EducationSessionId -> EducationSession -> EverySet EducationTopic -> Html Msg
viewTopicsContent language currentDate id session selectedTopics =
    let
        innerContent =
            div [ class "full content" ]
                [ viewQuestionLabel language Translate.HealthTopicsQuestion
                , viewCheckBoxMultipleSelectInput language
                    [ TopicTuberculosis
                    , TopicSTD
                    , TopicMentalHealth
                    , TopicMalaria
                    , TopicChildhoodIllnesses
                    , TopicMalnutrition
                    , TopicANCPostpartum
                    , TopicFamilyPlanning
                    , TopicGender
                    , TopicNCD
                    ]
                    []
                    (EverySet.toList selectedTopics)
                    Nothing
                    (ToggleEducationTopic selectedTopics)
                    Translate.EducationTopic
                ]

        saveButton =
            viewEncounterActionButton language
                Translate.Save
                "primary"
                (not <| EverySet.isEmpty selectedTopics)
                (SaveTopics session.participants selectedTopics)
    in
    div [ class "ui unstackable items" ]
        [ div [ class "ui full segment" ]
            [ innerContent
            , saveButton
            ]
        ]


viewParticipantsContent :
    Language
    -> NominalDate
    -> VillageId
    -> EducationSessionId
    -> EducationSession
    -> ModelIndexedDb
    -> Model
    -> EverySet PersonId
    -> Html Msg
viewParticipantsContent language currentDate villageId id session db model selectedParticipants =
    div [ class "search-wrapper" ]
        [ div [ class "ui full segment" ]
            [ viewSearchForm language
                currentDate
                villageId
                selectedParticipants
                db
                model
            , viewEncounterActionButton language
                Translate.RecordGroupEducation
                "primary"
                (not <| EverySet.isEmpty selectedParticipants)
                EndEncounter
            ]
        ]


viewSearchForm : Language -> NominalDate -> VillageId -> EverySet PersonId -> ModelIndexedDb -> Model -> Html Msg
viewSearchForm language currentDate villageId selectedParticipants db model =
    let
        filterForm =
            Pages.Utils.viewCustomNameFilter language model.filter SetFilter Translate.PlaceholderEnterParticipantName

        candidates =
            Dict.get villageId db.peopleInVillage
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.filter
                        (\_ filteredPerson ->
                            isPersonAnAdult currentDate filteredPerson
                                |> Maybe.withDefault False
                        )
                    )
                |> Maybe.withDefault Dict.empty

        resultsForView =
            if Dict.isEmpty candidates then
                [ div [ class "ui message warning" ]
                    [ text <| translate language Translate.EducationSessionNoCandidatesInVillage ]
                ]

            else
                let
                    results =
                        if String.isEmpty model.filter && model.initialResultsDisplay == InitialResultsHidden then
                            Dict.empty

                        else
                            candidates

                    filter =
                        normalizeFilter model.filter

                    filteredResults =
                        Dict.filter
                            (\_ filteredPerson ->
                                matchFilter filter filteredPerson.name
                            )
                            results
                in
                if
                    (not <| String.isEmpty model.filter)
                        && Dict.isEmpty filteredResults
                then
                    [ span [] [ text <| translate language Translate.NoMatchesFound ] ]

                else
                    Dict.map (viewParticipant selectedParticipants) filteredResults
                        |> Dict.values
    in
    div [ class "registration-page search" ]
        [ h3 [ class "ui header" ]
            [ text <| translate language Translate.CheckIn ]
        , div [ class "search-top" ]
            [ p [ class "search-helper" ]
                [ text <| translate language Translate.ClickTheCheckMarkEducationSesison ]
            , filterForm
            , viewToggleDisplay language model
            ]
        , div
            [ class "search-middle" ]
            [ div [ class "ui unstackable items participants-list" ]
                resultsForView
            ]
        ]


viewToggleDisplay : Language -> Model -> Html Msg
viewToggleDisplay language model =
    let
        ( label, action ) =
            if String.isEmpty model.filter then
                ( Translate.EducationSessionInitialResultsDisplay model.initialResultsDisplay
                , ToggleInitialResultsDisplay
                )

            else
                ( Translate.EducationSessionInitialResultsDisplay InitialResultsHidden
                , Reset
                )
    in
    div [ class "toggle-initial-display" ]
        [ span [] [ text <| translate language Translate.Or ]
        , span
            [ class "toggle-text"
            , onClick action
            ]
            [ text <| translate language label ]
        ]


viewParticipant : EverySet PersonId -> PersonId -> Person -> Html Msg
viewParticipant selectedParticipants participantId participant =
    let
        checkIn =
            if EverySet.member participantId selectedParticipants then
                span
                    [ class "button-checked-in"
                    , onClick <| ToggleAttendance selectedParticipants participantId
                    ]
                    [ span [ class "icon-checked-in" ] [] ]

            else
                span
                    [ class "button-check-in"
                    , onClick <| ToggleAttendance selectedParticipants participantId
                    ]
                    [ span [ class "icon-check-in" ] [] ]
    in
    div
        [ class "item" ]
        [ thumbnailImage "participant ui avatar image" participant.avatarUrl participant.name 110 110
        , div
            [ class "content" ]
            [ div [ class "header" ] [ text participant.name ]
            ]
        , checkIn
        ]
