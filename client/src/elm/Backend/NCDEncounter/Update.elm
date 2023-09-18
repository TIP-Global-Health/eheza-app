module Backend.NCDEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.NCDEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> NCDEncounterId
    -> Maybe NCDEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseNCDEncounter ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | endDate = Just currentDate }) model

        SetNCDDiagnoses diagnoses ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | diagnoses = diagnoses }) model

        HandleUpdatedNCDEncounter data ->
            ( { model | updateNCDEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDangerSigns personId valueId value ->
            ( { model | saveDangerSigns = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdDangerSignsEndpoint HandleSavedDangerSigns
            , []
            )

        HandleSavedDangerSigns data ->
            ( { model | saveDangerSigns = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomReview personId valueId value ->
            ( { model | saveSymptomReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdSymptomReviewEndpoint HandleSavedSymptomReview
            , []
            )

        HandleSavedSymptomReview data ->
            ( { model | saveSymptomReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFamilyPlanning personId valueId value ->
            ( { model | saveFamilyPlanning = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdFamilyPlanningEndpoint HandleSavedFamilyPlanning
            , []
            )

        HandleSavedFamilyPlanning data ->
            ( { model | saveFamilyPlanning = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCoreExam personId valueId value ->
            ( { model | saveCoreExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdCoreExamEndpoint HandleSavedCoreExam
            , []
            )

        HandleSavedCoreExam data ->
            ( { model | saveCoreExam = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdVitalsEndpoint HandleSavedVitals
            , []
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCoMorbidities personId valueId value ->
            ( { model | saveCoMorbidities = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdCoMorbiditiesEndpoint HandleSavedCoMorbidities
            , []
            )

        HandleSavedCoMorbidities data ->
            ( { model | saveCoMorbidities = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMedicationHistory personId valueId value ->
            ( { model | saveMedicationHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdMedicationHistoryEndpoint HandleSavedMedicationHistory
            , []
            )

        HandleSavedMedicationHistory data ->
            ( { model | saveMedicationHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSocialHistory personId valueId value ->
            ( { model | saveSocialHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdSocialHistoryEndpoint HandleSavedSocialHistory
            , []
            )

        HandleSavedSocialHistory data ->
            ( { model | saveSocialHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFamilyHistory personId valueId value ->
            ( { model | saveFamilyHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdFamilyHistoryEndpoint HandleSavedFamilyHistory
            , []
            )

        HandleSavedFamilyHistory data ->
            ( { model | saveFamilyHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveOutsideCare personId valueId value ->
            ( { model | saveOutsideCare = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdOutsideCareEndpoint HandleSavedOutsideCare
            , []
            )

        HandleSavedOutsideCare data ->
            ( { model | saveOutsideCare = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHIVTest personId valueId value ->
            ( { model | saveHIVTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdHIVTestEndpoint HandleSavedHIVTest
            , []
            )

        HandleSavedHIVTest data ->
            ( { model | saveHIVTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveUrineDipstickTest personId valueId value ->
            ( { model | saveUrineDipstickTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdUrineDipstickTestEndpoint HandleSavedUrineDipstickTest
            , []
            )

        HandleSavedUrineDipstickTest data ->
            ( { model | saveUrineDipstickTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveRandomBloodSugarTest personId valueId value ->
            ( { model | saveRandomBloodSugarTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdRandomBloodSugarTestEndpoint HandleSavedRandomBloodSugarTest
            , []
            )

        HandleSavedRandomBloodSugarTest data ->
            ( { model | saveRandomBloodSugarTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveLabsResults personId valueId value ->
            ( { model | saveLabsResults = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdLabsResultsEndpoint HandleSavedLabsResults
            , []
            )

        HandleSavedLabsResults data ->
            ( { model | saveLabsResults = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePregnancyTest personId valueId value ->
            ( { model | savePregnancyTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdPregnancyTestEndpoint HandleSavedPregnancyTest
            , []
            )

        HandleSavedPregnancyTest data ->
            ( { model | savePregnancyTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCreatinineTest personId valueId value ->
            ( { model | saveCreatinineTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdCreatinineTestEndpoint HandleSavedCreatinineTest
            , []
            )

        HandleSavedCreatinineTest data ->
            ( { model | saveCreatinineTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveLiverFunctionTest personId valueId value ->
            ( { model | saveLiverFunctionTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdLiverFunctionTestEndpoint HandleSavedLiverFunctionTest
            , []
            )

        HandleSavedLiverFunctionTest data ->
            ( { model | saveLiverFunctionTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveLipidPanelTest personId valueId value ->
            ( { model | saveLipidPanelTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdLipidPanelTestEndpoint HandleSavedLipidPanelTest
            , []
            )

        HandleSavedLipidPanelTest data ->
            ( { model | saveLipidPanelTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHbA1cTest personId valueId value ->
            ( { model | saveHbA1cTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdHbA1cTestEndpoint HandleSavedHbA1cTest
            , []
            )

        HandleSavedHbA1cTest data ->
            ( { model | saveHbA1cTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdHealthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMedicationDistribution personId valueId value ->
            ( { model | saveMedicationDistribution = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdMedicationDistributionEndpoint HandleSavedMedicationDistribution
            , []
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveReferral personId valueId value ->
            ( { model | saveReferral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdReferralEndpoint HandleSavedReferral
            , []
            )

        HandleSavedReferral data ->
            ( { model | saveReferral = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateEncounter :
    NominalDate
    -> NCDEncounterId
    -> Maybe NCDEncounter
    -> (NCDEncounter -> NCDEncounter)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateEncounter currentDate encounterId maybeEncounter updateFunc model =
    maybeEncounter
        |> unwrap ( model, Cmd.none, [] )
            (\encounter ->
                ( { model | updateNCDEncounter = Loading }
                , updateFunc encounter
                    |> sw.patchFull ncdEncounterEndpoint encounterId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedNCDEncounter)
                , []
                )
            )
