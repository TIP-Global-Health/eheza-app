module Pages.PatientRecord.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (generateFullName, isPersonAnAdult)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import Pages.WellChildEncounter.View exposing (thumbnailDimensions, viewPersonDetails)
import Pages.WellChildProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChildProgressReport.View exposing (viewProgressReport)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (spinner, thumbnailImage, viewModal)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    Dict.get id db.people
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\person ->
                if isPersonAnAdult currentDate person == Just False then
                    viewContentForChild language currentDate zscores id person isChw db model

                else
                    viewContentForAdult language currentDate person db model
            )
        |> Maybe.withDefault spinner


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CovidContactTracing ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PersonsPage Nothing ParticipantDirectoryOrigin
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContentForChild : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> Bool -> ModelIndexedDb -> Model -> Html Msg
viewContentForChild language currentDate zscores childId child isChw db model =
    let
        endEncounterData =
            Just <|
                { showEndEncounterDialog = False
                , allowEndEcounter = False
                , closeEncounterMsg = NoOp
                , setEndEncounterDialogStateMsg = always NoOp
                }

        mandatoryNutritionAssessmentMeasurementsTaken =
            False

        initiator =
            InitiatorPatientRecordChild childId
    in
    viewProgressReport language
        currentDate
        zscores
        isChw
        initiator
        mandatoryNutritionAssessmentMeasurementsTaken
        db
        model.diagnosisMode
        SetActivePage
        SetDiagnosisMode
        endEncounterData
        ( childId, child )


viewContentForAdult : Language -> NominalDate -> Person -> ModelIndexedDb -> Model -> Html Msg
viewContentForAdult language currentDate person db model =
    div [ class "page-activity patient-record" ]
        [ viewHeader language
        , div [ class "ui unstackable items" ]
            [ div [ class "item" ] <|
                viewPersonDetails
                    language
                    currentDate
                    person
            ]
        ]
