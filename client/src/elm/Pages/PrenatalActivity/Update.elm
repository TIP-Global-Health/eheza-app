module Pages.PrenatalActivity.Update exposing (update)

import App.Model
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , BreastExamSign(..)
        , CSectionReason(..)
        , DangerSign(..)
        , FamilyPlanningSign(..)
        , HandsCPESign(..)
        , LegsCPESign(..)
        , LungsCPESign(..)
        , NeckCPESign(..)
        , PreviousDeliveryPeriod(..)
        )
import Backend.Model
import Backend.PrenatalEncounter.Model
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils
    exposing
        ( toBreastExamValueWithDefault
        , toCorePhysicalExamValueWithDefault
        , toDangerSignsValueWithDefault
        , toFamilyPlanningValueWithDefault
        , toLastMenstrualPeriodValueWithDefault
        , toMedicalHistoryValueWithDefault
        , toMedicationValueWithDefault
        , toObstetricHistoryStep2ValueWithDefault
        , toObstetricHistoryValueWithDefault
        , toObstetricalExamValueWithDefault
        , toPrenatalNutritionValueWithDefault
        , toResourceValueWithDefault
        , toSocialHistoryValueWithDefault
        , toVitalsValueWithDefault
        )
import Result exposing (Result)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
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

        SetLmpRange value ->
            let
                range =
                    decodeLmpRange value

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

        SavePregnancyDating prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.pregnancyDatingData.form
                        |> toLastMenstrualPeriodValueWithDefault measurement
                        |> unwrap
                            []
                            (\lastMenstrualPeriodValue ->
                                [ Backend.PrenatalEncounter.Model.SaveLastMenstrualPeriod personId measurementId lastMenstrualPeriodValue
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
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
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormFirstStep

                                        updatedForm =
                                            { form | currentlyPregnant = Just value }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormFirstStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                ObstetricHistorySecondStep ->
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
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormFirstStep

                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    formUpdateFunc (Just number) form

                                                Err _ ->
                                                    formUpdateFunc Nothing form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormFirstStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                ObstetricHistorySecondStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveOBHistoryStep1 prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( appMsgs, updatedData ) =
                    case model.historyData.obstetricHistoryStep of
                        ObstetricHistoryFirstStep ->
                            ( model.historyData.obstetricFormFirstStep
                                |> toObstetricHistoryValueWithDefault measurement
                                |> unwrap
                                    []
                                    (\value ->
                                        [ Backend.PrenatalEncounter.Model.SaveObstetricHistory personId measurementId value
                                            |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                            |> App.Model.MsgIndexedDb
                                        ]
                                    )
                            , model.historyData
                                |> (\data -> { data | obstetricHistoryStep = ObstetricHistorySecondStep })
                            )

                        -- Satisfy compiler.
                        ObstetricHistorySecondStep ->
                            ( [], model.historyData )
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetCSectionReason reason ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormSecondStep

                                        updatedReason =
                                            if form.cSectionReason == Just reason then
                                                Nothing

                                            else
                                                Just reason

                                        updatedForm =
                                            { form | cSectionReason = updatedReason }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                ObstetricHistoryFirstStep ->
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
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormSecondStep

                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | cSections = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                ObstetricHistoryFirstStep ->
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
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        updatedForm =
                                            formUpdateFunc value model.historyData.obstetricFormSecondStep
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                ObstetricHistoryFirstStep ->
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
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormSecondStep

                                        updatedPeriod =
                                            if form.previousDeliveryPeriod == Just period then
                                                Nothing

                                            else
                                                Just period

                                        updatedForm =
                                            { form | previousDeliveryPeriod = updatedPeriod }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                ObstetricHistoryFirstStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        BackToOBHistoryStep1 ->
            let
                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricHistoryStep = ObstetricHistoryFirstStep })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveOBHistoryStep2 prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( appMsgs, updatedData ) =
                    case model.historyData.obstetricHistoryStep of
                        -- Satisfy compiler.
                        ObstetricHistoryFirstStep ->
                            ( [], model.historyData )

                        ObstetricHistorySecondStep ->
                            ( model.historyData.obstetricFormSecondStep
                                |> toObstetricHistoryStep2ValueWithDefault measurement
                                |> unwrap
                                    []
                                    (\value ->
                                        [ Backend.PrenatalEncounter.Model.SaveObstetricHistoryStep2 personId measurementId value
                                            |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                            |> App.Model.MsgIndexedDb
                                        ]
                                    )
                            , model.historyData
                                |> (\data -> { data | obstetricHistoryStep = ObstetricHistoryFirstStep, activeTask = Medical })
                            )
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , App.Model.ScrollToElement tasksBarId :: appMsgs
            )

        SetMedicalBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.historyData.medicalForm
                    in
                    model.historyData
                        |> (\data -> { data | medicalForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicalHistory prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.historyData.medicalForm
                        |> toMedicalHistoryValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveMedicalHistory personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = Social })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetSocialBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.historyData.socialForm
                    in
                    model.historyData
                        |> (\data -> { data | socialForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveSocialHistory prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.historyData.socialForm
                        |> toSocialHistoryValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveSocialHistory personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                                ]
                            )

                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = Obstetric })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
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

        SetVitalsIntMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            case String.toInt value of
                                Ok number ->
                                    formUpdateFunc (Just number) model.examinationData.vitalsForm

                                Err _ ->
                                    formUpdateFunc Nothing model.examinationData.vitalsForm
                    in
                    model.examinationData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsFloatMeasurement formUpdateFunc value ->
            let
                updatedData =
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
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitals prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.examinationData.vitalsForm
                        |> toVitalsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveVitals personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = NutritionAssessment })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetNutritionAssessmentMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            case String.toFloat value of
                                Ok number ->
                                    formUpdateFunc (Just number) model.examinationData.nutritionAssessmentForm

                                Err _ ->
                                    formUpdateFunc Nothing model.examinationData.nutritionAssessmentForm
                    in
                    model.examinationData
                        |> (\data -> { data | nutritionAssessmentForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutritionAssessment prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.examinationData.nutritionAssessmentForm
                        |> toPrenatalNutritionValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveNutrition personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = CorePhysicalExam })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetCorePhysicalExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.examinationData.corePhysicalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamHeart value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.corePhysicalExamForm
                                |> (\form -> { form | heart = Just value })
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamNeck value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.corePhysicalExamForm
                                |> (\form ->
                                        case form.neck of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | neck = updatedOptions }

                                                else
                                                    case value of
                                                        NormalNeck ->
                                                            { form | neck = Just [ value ] }

                                                        _ ->
                                                            let
                                                                updatedOptions =
                                                                    case options of
                                                                        [ NormalNeck ] ->
                                                                            Just [ value ]

                                                                        _ ->
                                                                            Just (value :: options)
                                                            in
                                                            { form | neck = updatedOptions }

                                            Nothing ->
                                                { form | neck = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamLungs value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.corePhysicalExamForm
                                |> (\form ->
                                        case form.lungs of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | lungs = updatedOptions }

                                                else
                                                    case value of
                                                        NormalLungs ->
                                                            { form | lungs = Just [ value ] }

                                                        _ ->
                                                            let
                                                                updatedOptions =
                                                                    case options of
                                                                        [ NormalLungs ] ->
                                                                            Just [ value ]

                                                                        _ ->
                                                                            Just (value :: options)
                                                            in
                                                            { form | lungs = updatedOptions }

                                            Nothing ->
                                                { form | lungs = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamAbdomen value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.corePhysicalExamForm
                                |> (\form ->
                                        case form.abdomen of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | abdomen = updatedOptions }

                                                else
                                                    case value of
                                                        NormalAbdomen ->
                                                            { form | abdomen = Just [ value ] }

                                                        _ ->
                                                            let
                                                                updatedOptions =
                                                                    case options of
                                                                        [ NormalAbdomen ] ->
                                                                            Just [ value ]

                                                                        _ ->
                                                                            Just (value :: options)
                                                            in
                                                            { form | abdomen = updatedOptions }

                                            Nothing ->
                                                { form | abdomen = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamHands value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.corePhysicalExamForm
                                |> (\form ->
                                        case form.hands of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | hands = updatedOptions }

                                                else
                                                    case value of
                                                        NormalHands ->
                                                            { form | hands = Just [ value ] }

                                                        _ ->
                                                            let
                                                                updatedOptions =
                                                                    case options of
                                                                        [ NormalHands ] ->
                                                                            Just [ value ]

                                                                        _ ->
                                                                            Just (value :: options)
                                                            in
                                                            { form | hands = updatedOptions }

                                            Nothing ->
                                                { form | hands = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamLegs value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.corePhysicalExamForm
                                |> (\form ->
                                        case form.legs of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | legs = updatedOptions }

                                                else
                                                    case value of
                                                        NormalLegs ->
                                                            { form | legs = Just [ value ] }

                                                        _ ->
                                                            let
                                                                updatedOptions =
                                                                    case options of
                                                                        [ NormalLegs ] ->
                                                                            Just [ value ]

                                                                        _ ->
                                                                            Just (value :: options)
                                                            in
                                                            { form | legs = updatedOptions }

                                            Nothing ->
                                                { form | legs = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveCorePhysicalExam prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.examinationData.corePhysicalExamForm
                        |> toCorePhysicalExamValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveCorePhysicalExam personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = ObstetricalExam })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetObstetricalExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.examinationData.obstetricalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamIntMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            case String.toInt value of
                                Ok number ->
                                    formUpdateFunc (Just number) model.examinationData.obstetricalExamForm

                                Err _ ->
                                    formUpdateFunc Nothing model.examinationData.obstetricalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamFloatMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            case String.toFloat value of
                                Ok number ->
                                    formUpdateFunc (Just number) model.examinationData.obstetricalExamForm

                                Err _ ->
                                    formUpdateFunc Nothing model.examinationData.obstetricalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamFetalPresentation value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.obstetricalExamForm
                                |> (\form ->
                                        case form.fetalPresentation of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | fetalPresentation = updatedOptions }

                                                else
                                                    { form | fetalPresentation = Just (value :: options) }

                                            Nothing ->
                                                { form | fetalPresentation = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamCSectionScar value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.obstetricalExamForm
                                |> (\form -> { form | cSectionScar = Just value })
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveObstetricalExam prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.examinationData.obstetricalExamForm
                        |> toObstetricalExamValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveObstetricalExam personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = BreastExam })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetBreastExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.examinationData.breastExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | breastExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetBreastExamBreast value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.breastExamForm
                                |> (\form ->
                                        case form.breast of
                                            Just options ->
                                                if List.member value options then
                                                    let
                                                        updatedOptions =
                                                            if List.length options == 1 then
                                                                Nothing

                                                            else
                                                                options |> List.filter ((/=) value) |> Just
                                                    in
                                                    { form | breast = updatedOptions }

                                                else
                                                    case value of
                                                        NormalBreast ->
                                                            { form | breast = Just [ value ] }

                                                        _ ->
                                                            let
                                                                updatedOptions =
                                                                    case options of
                                                                        [ NormalBreast ] ->
                                                                            Just [ value ]

                                                                        _ ->
                                                                            Just (value :: options)
                                                            in
                                                            { form | breast = updatedOptions }

                                            Nothing ->
                                                { form | breast = Just [ value ] }
                                   )
                    in
                    model.examinationData
                        |> (\data -> { data | breastExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveBreastExam prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.examinationData.breastExamForm
                        |> toBreastExamValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveBreastExam personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                                ]
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = Vitals })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetFamilyPlanningSign sign ->
            let
                form =
                    model.familyPlanningData.form

                updatedForm =
                    case form.signs of
                        Just signs ->
                            if List.member sign signs then
                                let
                                    updatedSigns =
                                        if List.length signs == 1 then
                                            Nothing

                                        else
                                            signs |> List.filter ((/=) sign) |> Just
                                in
                                { form | signs = updatedSigns }

                            else
                                case sign of
                                    NoFamilyPlanning ->
                                        { form | signs = Just [ sign ] }

                                    _ ->
                                        let
                                            updatedSigns =
                                                case signs of
                                                    [ NoFamilyPlanning ] ->
                                                        Just [ sign ]

                                                    _ ->
                                                        Just (sign :: signs)
                                        in
                                        { form | signs = updatedSigns }

                        Nothing ->
                            { form | signs = Just [ sign ] }

                updatedData =
                    model.familyPlanningData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | familyPlanningData = updatedData }
            , Cmd.none
            , []
            )

        SaveFamilyPlanning prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.familyPlanningData.form
                        |> toFamilyPlanningValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveFamilyPlanning personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActivePatientProvisionsTask task ->
            let
                updatedData =
                    model.patientProvisionsData
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | patientProvisionsData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.patientProvisionsData.medicationForm
                    in
                    model.patientProvisionsData
                        |> (\data -> { data | medicationForm = updatedForm })
            in
            ( { model | patientProvisionsData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedication prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.patientProvisionsData.medicationForm
                        |> toMedicationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveMedication personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                updatedData =
                    model.patientProvisionsData
                        |> (\data -> { data | activeTask = Resources })
            in
            ( { model | patientProvisionsData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetResourcesBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.patientProvisionsData.resourcesForm
                    in
                    model.patientProvisionsData
                        |> (\data -> { data | resourcesForm = updatedForm })
            in
            ( { model | patientProvisionsData = updatedData }
            , Cmd.none
            , []
            )

        SaveResources prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.patientProvisionsData.resourcesForm
                        |> toResourceValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveResource personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                                ]
                            )

                updatedData =
                    model.patientProvisionsData
                        |> (\data -> { data | activeTask = Medication })
            in
            ( { model | patientProvisionsData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetDangerSign sign ->
            let
                form =
                    model.dangerSignsData.form

                updatedForm =
                    case form.signs of
                        Just signs ->
                            if List.member sign signs then
                                let
                                    updatedSigns =
                                        if List.length signs == 1 then
                                            Nothing

                                        else
                                            signs |> List.filter ((/=) sign) |> Just
                                in
                                { form | signs = updatedSigns }

                            else
                                case sign of
                                    NoDangerSign ->
                                        { form | signs = Just [ sign ] }

                                    _ ->
                                        let
                                            updatedSigns =
                                                case signs of
                                                    [ NoDangerSign ] ->
                                                        Just [ sign ]

                                                    _ ->
                                                        Just (sign :: signs)
                                        in
                                        { form | signs = updatedSigns }

                        Nothing ->
                            { form | signs = Just [ sign ] }

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveDangerSigns prenatalEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.dangerSignsData.form
                        |> toDangerSignsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveDangerSigns personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter prenatalEncounterId
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
