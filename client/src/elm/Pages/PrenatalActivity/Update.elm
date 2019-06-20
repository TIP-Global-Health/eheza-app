module Pages.PrenatalActivity.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model exposing (ModelIndexedDb)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.PrenatalActivity.Model exposing (..)
import PrenatalActivity.Model exposing (PrenatalActivity)
import Result exposing (Result)


update : PersonId -> PrenatalActivity -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update motherId activity db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        ToggleDateSelector ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | isDateSelectorOpen = not form.isDateSelectorOpen })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpDate value ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpDate = Just value })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpDateConfident value ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpDateConfident = Just value })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpRange s ->
            let
                range =
                    decodeLmpRange s

                ( lmpDate, isDateSelectorOpen ) =
                    if isJust range then
                        ( model.pregnancyDatingData.form.lmpDate, True )

                    else
                        ( Nothing, False )

                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpRange = range, lmpDate = lmpDate, isDateSelectorOpen = isDateSelectorOpen })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetOBFirstStepCompleted ->
            let
                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricForm = SecondStep emptyObstetricFormSecondStep })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetHistoryTaskCompleted ->
            let
                updatedData =
                    let
                        activeTask =
                            case model.historyData.activeTask of
                                Obstetric ->
                                    Medical

                                Medical ->
                                    Social

                                Social ->
                                    Obstetric

                        completedTasks =
                            if List.member model.historyData.activeTask model.historyData.completedTasks then
                                model.historyData.completedTasks

                            else
                                model.historyData.activeTask :: model.historyData.completedTasks
                    in
                    model.historyData
                        |> (\data -> { data | activeTask = activeTask, completedTasks = completedTasks })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetActiveHistoryTask task ->
            let
                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetCurrentlyPregnant value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            { form | currentlyPregnant = Just value }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOBIntInput formUpdateFunc value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    formUpdateFunc (Just number) form

                                                Err _ ->
                                                    formUpdateFunc Nothing form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetCSectionReason reason ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                SecondStep form ->
                                    let
                                        updatedReason =
                                            if form.reasonForCSection == Just reason then
                                                Nothing

                                            else
                                                Just reason

                                        updatedForm =
                                            { form | reasonForCSection = updatedReason }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = SecondStep updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                FirstStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetNumberOfCSections value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                SecondStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | cSections = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = SecondStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                FirstStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOBBoolInput formUpdateFunc value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                SecondStep form ->
                                    let
                                        updatedForm =
                                            formUpdateFunc value form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = SecondStep updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                FirstStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetPreviousDeliveryPeriod period ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                SecondStep form ->
                                    let
                                        updatedPeriod =
                                            if form.previousDeliveryPeriod == Just period then
                                                Nothing

                                            else
                                                Just period

                                        updatedForm =
                                            { form | previousDeliveryPeriod = updatedPeriod }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = SecondStep updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                FirstStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicalBoolInput formUpdateFunc value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Medical ->
                            let
                                updatedForm =
                                    formUpdateFunc value model.historyData.medicalForm
                            in
                            model.historyData
                                |> (\data -> { data | medicalForm = updatedForm })

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetSocialBoolInput formUpdateFunc value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Social ->
                            let
                                updatedForm =
                                    formUpdateFunc value model.historyData.socialForm
                            in
                            model.historyData
                                |> (\data -> { data | socialForm = updatedForm })

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetExaminationTaskCompleted ->
            let
                updatedData =
                    let
                        activeTask =
                            case model.examinationData.activeTask of
                                Vitals ->
                                    NutritionAssessment

                                NutritionAssessment ->
                                    CorePhysicalExam

                                CorePhysicalExam ->
                                    ObstetricalExam

                                ObstetricalExam ->
                                    BreastExam

                                BreastExam ->
                                    Vitals

                        completedTasks =
                            if List.member model.examinationData.activeTask model.examinationData.completedTasks then
                                model.examinationData.completedTasks

                            else
                                model.examinationData.activeTask :: model.examinationData.completedTasks
                    in
                    model.examinationData
                        |> (\data -> { data | activeTask = activeTask, completedTasks = completedTasks })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetActiveExaminationTask task ->
            let
                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsMeasurement formUpdateFunc value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        Vitals ->
                            let
                                updatedForm =
                                    case String.toFloat value of
                                        Ok number ->
                                            formUpdateFunc (Just number) model.examinationData.vitalsForm

                                        Err _ ->
                                            formUpdateFunc Nothing model.examinationData.vitalsForm
                            in
                            model.examinationData
                                |> (\data -> { data | vitalsForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetNutritionAssessmentMeasurement formUpdateFunc value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        NutritionAssessment ->
                            let
                                updatedForm =
                                    case String.toFloat value of
                                        Ok number ->
                                            formUpdateFunc (Just number) model.examinationData.nutritionAssessmentForm
                                                |> calculateBmi

                                        Err _ ->
                                            formUpdateFunc Nothing model.examinationData.nutritionAssessmentForm
                                                |> calculateBmi
                            in
                            model.examinationData
                                |> (\data -> { data | nutritionAssessmentForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        CorePhysicalExam ->
                            let
                                updatedForm =
                                    formUpdateFunc value model.examinationData.corePhysicalExamForm
                            in
                            model.examinationData
                                |> (\data -> { data | corePhysicalExamForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamNeck value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        CorePhysicalExam ->
                            let
                                updatedForm =
                                    model.examinationData.corePhysicalExamForm
                                        |> (\form -> { form | neck = Just value })
                            in
                            model.examinationData
                                |> (\data -> { data | corePhysicalExamForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamLungs value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        CorePhysicalExam ->
                            let
                                updatedForm =
                                    model.examinationData.corePhysicalExamForm
                                        |> (\form -> { form | lungs = Just value })
                            in
                            model.examinationData
                                |> (\data -> { data | corePhysicalExamForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamAbdomen value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        CorePhysicalExam ->
                            let
                                updatedForm =
                                    model.examinationData.corePhysicalExamForm
                                        |> (\form -> { form | abdomen = Just value })
                            in
                            model.examinationData
                                |> (\data -> { data | corePhysicalExamForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamHands value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        CorePhysicalExam ->
                            let
                                updatedForm =
                                    model.examinationData.corePhysicalExamForm
                                        |> (\form -> { form | hands = Just value })
                            in
                            model.examinationData
                                |> (\data -> { data | corePhysicalExamForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamLegs value ->
            let
                updatedData =
                    case model.examinationData.activeTask of
                        CorePhysicalExam ->
                            let
                                updatedForm =
                                    model.examinationData.corePhysicalExamForm
                                        |> (\form -> { form | legs = Just value })
                            in
                            model.examinationData
                                |> (\data -> { data | corePhysicalExamForm = updatedForm })

                        _ ->
                            model.examinationData
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )


calculateBmi : NutritionAssessmentForm -> NutritionAssessmentForm
calculateBmi form =
    if isNothing form.weight || isNothing form.height then
        { form | bmi = Nothing }

    else
        let
            height =
                form.height |> Maybe.withDefault 0

            weight =
                form.weight |> Maybe.withDefault 0

            bmi =
                weight / ((height / 100) ^ 2)
        in
        { form | bmi = Just bmi }
