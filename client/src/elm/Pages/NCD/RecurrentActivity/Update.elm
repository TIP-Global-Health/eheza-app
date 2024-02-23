module Pages.NCD.RecurrentActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Measurement.Utils
    exposing
        ( toCreatinineResultValueWithDefault
        , toLipidPanelResultValueWithDefault
        , toLiverFunctionResultValueWithDefault
        , toRandomBloodSugarResultValueWithDefault
        , toUrineDipstickResultValueWithDefault
        )
import Pages.GlobalCaseManagement.Utils exposing (labsResultsTestData)
import Pages.NCD.RecurrentActivity.Model exposing (..)
import Pages.NCD.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (setMultiSelectInputValue)
import RemoteData


update : NominalDate -> NCDEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        medicationDistributionForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.medicationDistribution
                        >> getMeasurementValueFunc
                        >> medicationDistributionFormWithDefault model.nextStepsData.medicationDistributionForm
                    )
                |> Maybe.withDefault model.nextStepsData.medicationDistributionForm

        generateLabResultsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveLabResultsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDRecurrentEncounterPage id ]

        generateNextStepsMsgs personId nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault
                    (let
                        -- When Next steps are completed, and all lab results were
                        -- entered, we close the entry.
                        closeLabsResultsMsg =
                            Dict.get id db.ncdMeasurements
                                |> Maybe.andThen RemoteData.toMaybe
                                |> Maybe.andThen .labsResults
                                |> Maybe.map
                                    (\( labsResultsId, results ) ->
                                        let
                                            ( performedTests, completedTests ) =
                                                labsResultsTestData currentDate results
                                        in
                                        if EverySet.size performedTests == EverySet.size completedTests then
                                            let
                                                updatedValue =
                                                    results.value
                                                        |> (\value ->
                                                                { value | resolutionDate = currentDate }
                                                           )
                                            in
                                            [ CloseLabsResultsEntry personId labsResultsId updatedValue ]

                                        else
                                            []
                                    )
                                |> Maybe.withDefault []
                     in
                     (SetActivePage <| UserPage <| NCDRecurrentEncounterPage id) :: closeLabsResultsMsg
                    )
    in
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetActiveLabResultsTask task ->
            let
                updatedData =
                    model.labResultsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetProtein value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | protein = proteinValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetPH value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | ph = phValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetGlucose value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | glucose = glucoseValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetLeukocytes value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | leukocytes = leukocytesValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetNitrite value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | nitrite = nitriteValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetUrobilinogen value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | urobilinogen = urobilinogenValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHaemoglobin value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | haemoglobin = haemoglobinValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetKetone value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | ketone = ketoneValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetBilirubin value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | bilirubin = bilirubinValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveUrineDipstickResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toUrineDipstickResultValueWithDefault measurement model.labResultsData.urineDipstickTestForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveUrineDipstickTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetRandomBloodSugar value ->
            let
                form =
                    model.labResultsData.randomBloodSugarTestForm

                updatedForm =
                    { form | sugarCount = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveRandomBloodSugarResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toRandomBloodSugarResultValueWithDefault measurement model.labResultsData.randomBloodSugarTestForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveRandomBloodSugarTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetCreatinineResult value ->
            let
                form =
                    model.labResultsData.creatinineTestForm

                updatedForm =
                    { form | creatinineResult = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | creatinineTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetBUNResult value ->
            let
                form =
                    model.labResultsData.creatinineTestForm

                updatedForm =
                    { form | bunResult = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | creatinineTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveCreatinineResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toCreatinineResultValueWithDefault measurement model.labResultsData.creatinineTestForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveCreatinineTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetAltResult value ->
            let
                form =
                    model.labResultsData.liverFunctionTestForm

                updatedForm =
                    { form | altResult = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | liverFunctionTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetAstResult value ->
            let
                form =
                    model.labResultsData.liverFunctionTestForm

                updatedForm =
                    { form | astResult = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | liverFunctionTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveLiverFunctionResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toLiverFunctionResultValueWithDefault measurement model.labResultsData.liverFunctionTestForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveLiverFunctionTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetUnitOfMeasurement value ->
            let
                form =
                    model.labResultsData.lipidPanelTestForm

                updatedForm =
                    { form
                        | unitOfMeasurement = unitOfMeasurementFromString value
                        , totalCholesterolResult = Nothing
                        , totalCholesterolResultDirty = True
                        , ldlCholesterolResult = Nothing
                        , ldlCholesterolResultDirty = True
                        , hdlCholesterolResult = Nothing
                        , hdlCholesterolResultDirty = True
                        , triglyceridesResult = Nothing
                        , triglyceridesResultDirty = True
                    }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetTotalCholesterolResult value ->
            let
                form =
                    model.labResultsData.lipidPanelTestForm

                updatedForm =
                    { form | totalCholesterolResult = String.toFloat value, totalCholesterolResultDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetLDLCholesterolResult value ->
            let
                form =
                    model.labResultsData.lipidPanelTestForm

                updatedForm =
                    { form | ldlCholesterolResult = String.toFloat value, ldlCholesterolResultDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHDLCholesterolResult value ->
            let
                form =
                    model.labResultsData.lipidPanelTestForm

                updatedForm =
                    { form | hdlCholesterolResult = String.toFloat value, hdlCholesterolResultDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetTriglyceridesResult value ->
            let
                form =
                    model.labResultsData.lipidPanelTestForm

                updatedForm =
                    { form | triglyceridesResult = String.toFloat value, triglyceridesResultDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveLipidPanelResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toLipidPanelResultValueWithDefault measurement model.labResultsData.lipidPanelTestForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveLipidPanelTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetActiveNextStepsTask task ->
            let
                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendedTreatmentSignSingle allowedSigns sign ->
            let
                updatedSigns =
                    -- Since we may have values from recurrent phase of encounter, we make
                    -- sure to preserve them, before setting new value at inital phase.
                    Maybe.map
                        (\signs ->
                            List.filter (\sign_ -> not <| List.member sign_ allowedSigns) signs
                                |> List.append [ sign ]
                        )
                        medicationDistributionForm.recommendedTreatmentSigns
                        |> Maybe.withDefault [ sign ]

                updatedForm =
                    { medicationDistributionForm | recommendedTreatmentSigns = Just updatedSigns }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendedTreatmentSignMultiple allowedSigns noneValue sign ->
            let
                ( currentSigns, preservedSigns ) =
                    Maybe.map
                        (\signs ->
                            List.partition (\sign_ -> List.member sign_ allowedSigns) signs
                        )
                        medicationDistributionForm.recommendedTreatmentSigns
                        |> Maybe.withDefault ( [], [] )

                form =
                    { signs = Just currentSigns }

                formAfterUpdate =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        noneValue
                        sign
                        form

                updatedSigns =
                    Maybe.withDefault [] formAfterUpdate.signs
                        |> List.append preservedSigns

                updatedForm =
                    { medicationDistributionForm | recommendedTreatmentSigns = Just updatedSigns }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationDistributionBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.nextStepsData.medicationDistributionForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicationDistribution personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs personId nextTask

                appMsgs =
                    toMedicationDistributionValueWithDefault measurement model.nextStepsData.medicationDistributionForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveMedicationDistribution personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetReferralBoolInput updateFunc value ->
            let
                updatedForm =
                    updateFunc value model.nextStepsData.referralForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetFacilityNonReferralReason currentValue facility reason ->
            let
                referralForm =
                    Dict.get id db.ncdMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.referral
                                >> getMeasurementValueFunc
                                >> referralFormWithDefault model.nextStepsData.referralForm
                            )
                        |> Maybe.withDefault model.nextStepsData.referralForm

                updatedValue =
                    nonReferralReasonToSign facility reason

                updatedNonReferralReasons =
                    Maybe.map
                        (\nonReferralReasons ->
                            case currentValue of
                                Just value ->
                                    EverySet.remove (nonReferralReasonToSign facility value) nonReferralReasons
                                        |> EverySet.insert updatedValue

                                Nothing ->
                                    EverySet.insert updatedValue nonReferralReasons
                        )
                        referralForm.nonReferralReasons
                        |> Maybe.withDefault (EverySet.singleton updatedValue)

                updatedForm =
                    { referralForm | nonReferralReasons = Just updatedNonReferralReasons }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveReferral personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs personId nextTask

                appMsgs =
                    toReferralValueWithDefault measurement model.nextStepsData.referralForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveReferral personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        CloseLabsResultsEntry personId labsResultsId value ->
            ( model
            , Cmd.none
            , [ Backend.NCDEncounter.Model.SaveLabsResults personId (Just labsResultsId) value
                    |> Backend.Model.MsgNCDEncounter id
                    |> App.Model.MsgIndexedDb
              ]
            )
