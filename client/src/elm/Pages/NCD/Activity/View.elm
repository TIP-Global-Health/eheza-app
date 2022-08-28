module Pages.NCD.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Utils exposing (..)
import Pages.NCD.Encounter.Model exposing (AssembledData)
import Pages.NCD.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( taskCompleted
        , viewCheckBoxMultipleSelectInput
        , viewCustomLabel
        , viewLabel
        , viewPersonDetailsExtended
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> NCDEncounterId -> NCDActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity ncd" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate activity db model assembled
        ]


viewHeader : Language -> NCDEncounterId -> NCDActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.NCDActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NCDEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> NCDActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled db model
        )


viewActivity : Language -> NominalDate -> NCDActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        DangerSigns ->
            viewDangerSignsContent language currentDate assembled model.dangerSignsData

        Examination ->
            -- @todo
            -- viewExaminationContent language currentDate assembled model.examinationData
            []

        FamilyPlanning ->
            -- @todo
            -- viewFamilyPlanningContent language currentDate assembled model.familyPlanningData
            []

        Laboratory ->
            -- @todo
            -- viewLaboratoryContent language currentDate assembled model.laboratoryData
            []

        MedicalHistory ->
            -- @todo
            -- viewMedicalHistoryContent language currentDate assembled model.medicalHistoryData
            []

        SymptomReview ->
            viewSymptomReviewContent language currentDate assembled model.symptomReviewData

        NextSteps ->
            -- @todo
            -- viewNextStepsContent language currentDate isChw assembled model.nextStepsData
            []


viewDangerSignsContent : Language -> NominalDate -> AssembledData -> DangerSignsData -> List (Html Msg)
viewDangerSignsContent language currentDate assembled data =
    let
        form =
            assembled.measurements.dangerSigns
                |> getMeasurementValueFunc
                |> dangerSignsFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language Translate.PatientGotAnyDangerSigns
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ Dyspnea, VisionChanges, ChestPain, FlankPain, Hematuria, SevereHeadaches, LossOfConciousness ]
                    []
                    (form.signs |> Maybe.withDefault [])
                    (Just NoNCDDangerSigns)
                    SetDangerSign
                    Translate.NCDDangerSign
              ]
            , taskCompleted form.signs
            , 1
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveDangerSigns assembled.participant.person assembled.measurements.dangerSigns
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewSymptomReviewContent : Language -> NominalDate -> AssembledData -> SymptomReviewData -> List (Html Msg)
viewSymptomReviewContent language currentDate assembled data =
    let
        form =
            assembled.measurements.symptomReview
                |> getMeasurementValueFunc
                |> symptomReviewFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language Translate.PatientGotAnySymptoms
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ SwellingInLegs, UrinaryFrequency, Anxiety, WeightLoss, Palpitations, Tremor ]
                    [ SwellingInFace, SwellingInAbdomen, DizzinessWithChangingPosition, MildHeadache ]
                    (form.group1Symptoms |> Maybe.withDefault [])
                    (Just NoNCDGroup1Symptoms)
                    SetGroup1Symptom
                    Translate.NCDGroup1Symptom
              , viewQuestionLabel language Translate.PatientGotPainAnywhewre
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ PainFlank, PainLowerBack, PainFeet ]
                    [ PainAbdomen, PainNeck ]
                    (form.painSymptoms |> Maybe.withDefault [])
                    (Just NoNCDPainSymptoms)
                    SetPainSymptom
                    Translate.NCDPainSymptom
              , viewQuestionLabel language Translate.PatientGotAnySymptoms
              , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
              , viewCheckBoxMultipleSelectInput language
                    [ WeaknessOfOneSideOfTheBody
                    , ProblemsWithWalking
                    , ProblemsWithTalking
                    , DecreasedVision
                    , BlurryVision
                    , IncreasedFatigueWithDailyActivities
                    ]
                    [ ShortOfBreathWhenLayingDown
                    , ShortOfBreathAtNight
                    , KidneyProblems
                    , NCDIncreasedThirst
                    ]
                    (form.group2Symptoms |> Maybe.withDefault [])
                    (Just NoNCDGroup2Symptoms)
                    SetGroup2Symptom
                    Translate.NCDGroup2Symptom
              ]
            , taskCompleted form.group1Symptoms + taskCompleted form.painSymptoms + taskCompleted form.group2Symptoms
            , 3
            )

        -- PatientGotPainAnywhewre
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form symptom-review" ] inputs
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveSymptomReview assembled.participant.person assembled.measurements.symptomReview
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]
