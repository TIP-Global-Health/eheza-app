module Pages.NCD.RecurrentActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (IllnessSymptom(..), ViralLoadStatus(..))
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model
import Backend.NCDEncounter.Model
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils
    exposing
        ( toRandomBloodSugarResultsValueWithDefault
        , toSendToHCValueWithDefault
        , toUrineDipstickResultsValueWithDefault
        )
import Pages.GlobalCaseManagement.Utils exposing (prenatalLabsResultsTestData)
import Pages.NCD.RecurrentActivity.Model exposing (..)
import Pages.NCD.RecurrentActivity.Utils exposing (..)
import Pages.NCD.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (nonAdministrationReasonToSign, setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)


update : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update language currentDate id db msg model =
    let
        noChange =
            ( model, Cmd.none, [] )

        generateLabResultsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveLabResultsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDRecurrentEncounterPage id ]

        -- generateNextStepsMsgs personId nextTask =
        --     Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
        --         |> Maybe.withDefault
        --             (let
        --                 -- When Next steps are completed, and all lab results were
        --                 -- entered, we close the entry.
        --                 closeLabsResultsMsg =
        --                     Dict.get id db.prenatalMeasurements
        --                         |> Maybe.andThen RemoteData.toMaybe
        --                         |> Maybe.andThen .labsResults
        --                         |> Maybe.map
        --                             (\( labsResultsId, results ) ->
        --                                 let
        --                                     ( performedTests, completedTests ) =
        --                                         prenatalLabsResultsTestData currentDate results
        --                                 in
        --                                 if List.length performedTests == List.length completedTests then
        --                                     let
        --                                         updatedValue =
        --                                             results.value
        --                                                 |> (\value ->
        --                                                         { value | resolutionDate = currentDate }
        --                                                    )
        --                                     in
        --                                     [ CloseLabsResultsEntry personId labsResultsId updatedValue ]
        --
        --                                 else
        --                                     []
        --                             )
        --                         |> Maybe.withDefault []
        --              in
        --              [ SetActivePage <|
        --                 UserPage <|
        --                     ClinicalProgressReportPage (Backend.NCDEncounter.Model.InitiatorRecurrentEncounterPage id) id
        --              ]
        --                 ++ closeLabsResultsMsg
        --             )
    in
    case msg of
        NoOp ->
            noChange

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
                    toUrineDipstickResultsValueWithDefault measurement model.labResultsData.urineDipstickTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    toRandomBloodSugarResultsValueWithDefault measurement model.labResultsData.randomBloodSugarTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

        -- SetActiveNextStepsTask task ->
        --     let
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | activeTask = Just task })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SetReferralBoolInput updateFunc value ->
        --     let
        --         updatedForm =
        --             updateFunc value model.nextStepsData.referralForm
        --
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | referralForm = updatedForm })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SetFacilityNonReferralReason currentValue facility reason ->
        --     let
        --         referralForm =
        --             Dict.get id db.prenatalMeasurements
        --                 |> Maybe.andThen RemoteData.toMaybe
        --                 |> Maybe.map
        --                     (.sendToHC
        --                         >> getMeasurementValueFunc
        --                         >> referralFormWithDefault model.nextStepsData.referralForm
        --                     )
        --                 |> Maybe.withDefault model.nextStepsData.referralForm
        --
        --         updatedValue =
        --             nonReferralReasonToSign facility reason
        --
        --         updatedFacilityNonReferralReasons =
        --             Maybe.map
        --                 (\facilityNonReferralReasons ->
        --                     case currentValue of
        --                         Just value ->
        --                             EverySet.remove (nonReferralReasonToSign facility value) facilityNonReferralReasons
        --                                 |> EverySet.insert updatedValue
        --
        --                         Nothing ->
        --                             EverySet.insert updatedValue facilityNonReferralReasons
        --                 )
        --                 referralForm.facilityNonReferralReasons
        --                 |> Maybe.withDefault (EverySet.singleton updatedValue)
        --
        --         updatedForm =
        --             { referralForm | facilityNonReferralReasons = Just updatedFacilityNonReferralReasons }
        --
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | referralForm = updatedForm })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SaveSendToHC personId saved nextTask ->
        --     let
        --         measurementId =
        --             Maybe.map Tuple.first saved
        --
        --         measurement =
        --             getMeasurementValueFunc saved
        --
        --         extraMsgs =
        --             generateNextStepsMsgs personId nextTask
        --
        --         appMsgs =
        --             model.nextStepsData.referralForm
        --                 |> toNCDReferralValueWithDefault measurement
        --                 |> Maybe.map
        --                     (Backend.NCDEncounter.Model.SaveSendToHC personId measurementId
        --                         >> Backend.Model.MsgNCDEncounter id
        --                         >> App.Model.MsgIndexedDb
        --                         >> List.singleton
        --                     )
        --                 |> Maybe.withDefault []
        --     in
        --     ( model
        --     , Cmd.none
        --     , appMsgs
        --     )
        --         |> sequenceExtra (update language currentDate id db) extraMsgs
        --
        -- SetMedicationDistributionBoolInput formUpdateFunc value ->
        --     let
        --         updatedForm =
        --             formUpdateFunc value model.nextStepsData.medicationDistributionForm
        --
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | medicationDistributionForm = updatedForm })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SetMedicationDistributionAdministrationNote currentValue medication reason ->
        --     let
        --         updatedValue =
        --             nonAdministrationReasonToSign medication reason
        --
        --         updatedNonAdministrationSigns =
        --             medicationDistributionForm.nonAdministrationSigns
        --                 |> Maybe.map
        --                     (\nonAdministrationSigns ->
        --                         case currentValue of
        --                             Just value ->
        --                                 EverySet.remove (nonAdministrationReasonToSign medication value) nonAdministrationSigns
        --                                     |> EverySet.insert updatedValue
        --
        --                             Nothing ->
        --                                 EverySet.insert updatedValue nonAdministrationSigns
        --                     )
        --                 |> Maybe.withDefault (EverySet.singleton updatedValue)
        --
        --         updatedForm =
        --             { medicationDistributionForm | nonAdministrationSigns = Just updatedNonAdministrationSigns }
        --
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | medicationDistributionForm = updatedForm })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SetRecommendedTreatmentSign allowedSigns sign ->
        --     let
        --         updatedSigns =
        --             -- Since we may have values from inital phase of encounter, we make
        --             -- sure to preserve them, before setting new value at recurrent phase.
        --             Maybe.map
        --                 (\signs ->
        --                     List.filter (\sign_ -> not <| List.member sign_ allowedSigns) signs
        --                         |> List.append [ sign ]
        --                 )
        --                 medicationDistributionForm.recommendedTreatmentSigns
        --                 |> Maybe.withDefault [ sign ]
        --
        --         updatedForm =
        --             { medicationDistributionForm | recommendedTreatmentSigns = Just updatedSigns }
        --
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | medicationDistributionForm = updatedForm })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SaveMedicationDistribution personId saved nextTask ->
        --     let
        --         measurementId =
        --             Maybe.map Tuple.first saved
        --
        --         measurement =
        --             getMeasurementValueFunc saved
        --
        --         extraMsgs =
        --             generateNextStepsMsgs personId nextTask
        --
        --         appMsgs =
        --             model.nextStepsData.medicationDistributionForm
        --                 |> toMedicationDistributionValueWithDefaultRecurrentPhase measurement
        --                 |> Maybe.map
        --                     (Backend.NCDEncounter.Model.SaveMedicationDistribution personId measurementId
        --                         >> Backend.Model.MsgNCDEncounter id
        --                         >> App.Model.MsgIndexedDb
        --                         >> List.singleton
        --                     )
        --                 |> Maybe.withDefault []
        --     in
        --     ( model
        --     , Cmd.none
        --     , appMsgs
        --     )
        --         |> sequenceExtra (update language currentDate id db) extraMsgs
        --
        -- SetHealthEducationBoolInput formUpdateFunc value ->
        --     let
        --         updatedForm =
        --             formUpdateFunc value model.nextStepsData.healthEducationForm
        --
        --         updatedData =
        --             model.nextStepsData
        --                 |> (\data -> { data | healthEducationForm = updatedForm })
        --     in
        --     ( { model | nextStepsData = updatedData }
        --     , Cmd.none
        --     , []
        --     )
        --
        -- SaveHealthEducation personId saved nextTask ->
        --     let
        --         measurementId =
        --             Maybe.map Tuple.first saved
        --
        --         measurement =
        --             getMeasurementValueFunc saved
        --
        --         extraMsgs =
        --             generateNextStepsMsgs personId nextTask
        --
        --         appMsgs =
        --             model.nextStepsData.healthEducationForm
        --                 |> toHealthEducationValueWithDefault measurement
        --                 |> Maybe.map
        --                     (Backend.NCDEncounter.Model.SaveHealthEducation personId measurementId
        --                         >> Backend.Model.MsgNCDEncounter id
        --                         >> App.Model.MsgIndexedDb
        --                         >> List.singleton
        --                     )
        --                 |> Maybe.withDefault []
        --     in
        --     ( model
        --     , Cmd.none
        --     , appMsgs
        --     )
        --         |> sequenceExtra (update language currentDate id db) extraMsgs
        CloseLabsResultsEntry personId labsResultsId value ->
            ( model
            , Cmd.none
            , [ Backend.NCDEncounter.Model.SaveLabsResults personId (Just labsResultsId) value
                    |> Backend.Model.MsgNCDEncounter id
                    |> App.Model.MsgIndexedDb
              ]
            )
