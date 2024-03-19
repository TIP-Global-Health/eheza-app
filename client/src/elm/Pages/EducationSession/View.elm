module Pages.EducationSession.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
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
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> EducationSessionId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        session =
            Dict.get id db.educationSessions
                |> Maybe.withDefault NotAsked
    in
    viewWebData language (viewHeaderAndContent language currentDate id model) identity session


viewHeaderAndContent : Language -> NominalDate -> EducationSessionId -> Model -> EducationSession -> Html Msg
viewHeaderAndContent language currentDate id model session =
    let
        header =
            viewHeader language viewMode session

        content =
            case viewMode of
                ModeTopics topics ->
                    viewTopicsContent language currentDate id session topics

                ModeAttendance participants ->
                    viewParticipantsContent language currentDate session participants

        viewMode =
            Maybe.withDefault
                (if EverySet.isEmpty session.topics then
                    ModeTopics session.topics

                 else
                    ModeAttendance session.participants
                )
                model.viewMode
    in
    div [ class "page-activity education-seesion" ]
        [ header
        , div [ class "ui unstackable items" ]
            [ content ]
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
                    (SetEducationTopic topics)
                    Translate.EducationTopic
                ]

        saveButton =
            viewEncounterActionButton language
                Translate.Save
                "primary"
                (not <| EverySet.isEmpty topics)
                (SaveTopics id { session | topics = topics })
    in
    div [ class "ui full segment" ]
        [ innerContent
        , saveButton
        ]


viewParticipantsContent : Language -> NominalDate -> EducationSession -> EverySet PersonId -> Html Msg
viewParticipantsContent language currentDate session participants =
    emptyNode
