module Pages.EducationSession.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isPersonAnAdult)
import Backend.Village.Utils exposing (personLivesInVillage)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.EducationSession.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewCheckBoxMultipleSelectInput, viewEncounterActionButton, viewQuestionLabel)
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
                    viewParticipantsContent language currentDate villageId session db model participants

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
viewTopicsContent language currentDate id session topics =
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
                    (EverySet.toList topics)
                    Nothing
                    (ToggleEducationTopic topics)
                    Translate.EducationTopic
                ]

        saveButton =
            viewEncounterActionButton language
                Translate.Save
                "primary"
                (not <| EverySet.isEmpty topics)
                (SaveTopics id { session | topics = topics })
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
    -> EducationSession
    -> ModelIndexedDb
    -> Model
    -> EverySet PersonId
    -> Html Msg
viewParticipantsContent language currentDate villageId session db model participants =
    div [ class "search-wrapper" ]
        [ div [ class "ui full segment" ]
            [ viewSearchForm language
                currentDate
                villageId
                participants
                db
                model
            ]
        ]



-- div [ class "ui full segment" ]
--     [ div [ class "ui full blue segment" ]
--         [ h3 [ class "ui header" ]
--             [ text <| translate language Translate.CheckIn ]
--         , p [] [ text <| translate language Translate.ClickTheCheckMark ]
--         , viewNameFilter language model.filter SetFilter
--         , viewToggleDisplay language model
--         , div [ class "search-middle" ]
--             [ div [ class "ui middle aligned divided list" ] mothers ]
--         , div [ class "search-bottom" ]
--             [ div
--                 [ class "register-actions" ]
--                 [ button
--                     [ class "ui primary button fluid"
--                     , onClick <| SetActivePage <| UserPage <| PersonsPage Nothing (GroupEncounterOrigin sessionId)
--                     ]
--                     [ text <| translate language Translate.AddNewParticipant ]
--                 ]
--             ]
--         ]
--     ]


viewSearchForm : Language -> NominalDate -> VillageId -> EverySet PersonId -> ModelIndexedDb -> Model -> Html Msg
viewSearchForm language currentDate villageId participants db model =
    let
        searchForm =
            Pages.Utils.viewSearchForm language model.input Translate.PlaceholderEnterParticipantName SetInput

        searchValue =
            Maybe.withDefault "" model.search

        results =
            if String.isEmpty searchValue then
                Nothing

            else
                Dict.get searchValue db.personSearches
                    |> Maybe.withDefault NotAsked
                    |> RemoteData.map
                        (Dict.filter
                            (\_ filteredPerson ->
                                personLivesInVillage filteredPerson db villageId
                                    && (isPersonAnAdult currentDate filteredPerson
                                            |> Maybe.withDefault False
                                       )
                            )
                        )
                    |> Just

        summary =
            let
                viewSummary data =
                    Dict.size data
                        |> Translate.ReportResultsOfParticipantsSearch
                        |> translate language
                        |> text
            in
            Maybe.map (viewWebData language viewSummary identity) results
                |> Maybe.withDefault emptyNode

        searchResultsParticipants =
            Maybe.withDefault (Success Dict.empty) results
                |> RemoteData.withDefault Dict.empty
                |> Dict.map (viewParticipant participants)
                |> Dict.values

        searchHelper =
            Translate.ClickTheCheckMarkEducationSesison
    in
    div [ class "registration-page search" ]
        [ h3 [ class "ui header" ]
            [ text <| translate language Translate.CheckIn ]
        , div [ class "search-top" ]
            [ p [ class "search-helper" ]
                [ text <| translate language searchHelper ]
            , searchForm
            ]
        , div
            [ class "search-middle" ]
            [ div
                [ class "results-summary" ]
                [ summary ]
            , div
                [ class "ui unstackable items participants-list" ]
                searchResultsParticipants
            ]
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
