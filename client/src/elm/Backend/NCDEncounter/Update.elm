module Backend.NCDEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.NCDEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> NCDEncounterId -> Maybe NCDEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseNCDEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeNCDEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull ncdEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedNCDEncounter)
                        )
                    )

        HandleClosedNCDEncounter data ->
            ( { model | closeNCDEncounter = data }
            , Cmd.none
            )

        SaveDangerSigns personId valueId value ->
            ( { model | saveDangerSigns = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdDangerSignsEndpoint HandleSavedDangerSigns
            )

        HandleSavedDangerSigns data ->
            ( { model | saveDangerSigns = data }
            , Cmd.none
            )

        SaveSymptomReview personId valueId value ->
            ( { model | saveSymptomReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdSymptomReviewEndpoint HandleSavedSymptomReview
            )

        HandleSavedSymptomReview data ->
            ( { model | saveSymptomReview = data }
            , Cmd.none
            )

        SaveFamilyPlanning personId valueId value ->
            ( { model | saveFamilyPlanning = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdFamilyPlanningEndpoint HandleSavedFamilyPlanning
            )

        HandleSavedFamilyPlanning data ->
            ( { model | saveFamilyPlanning = data }
            , Cmd.none
            )

        SaveCoreExam personId valueId value ->
            ( { model | saveCoreExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdCoreExamEndpoint HandleSavedCoreExam
            )

        HandleSavedCoreExam data ->
            ( { model | saveCoreExam = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdVitalsEndpoint HandleSavedVitals
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )

        SaveCoMorbidities personId valueId value ->
            ( { model | saveCoMorbidities = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdCoMorbiditiesEndpoint HandleSavedCoMorbidities
            )

        HandleSavedCoMorbidities data ->
            ( { model | saveCoMorbidities = data }
            , Cmd.none
            )

        SaveMedicationHistory personId valueId value ->
            ( { model | saveMedicationHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdMedicationHistoryEndpoint HandleSavedMedicationHistory
            )

        HandleSavedMedicationHistory data ->
            ( { model | saveMedicationHistory = data }
            , Cmd.none
            )

        SaveSocialHistory personId valueId value ->
            ( { model | saveSocialHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdSocialHistoryEndpoint HandleSavedSocialHistory
            )

        HandleSavedSocialHistory data ->
            ( { model | saveSocialHistory = data }
            , Cmd.none
            )

        SaveFamilyHistory personId valueId value ->
            ( { model | saveFamilyHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdFamilyHistoryEndpoint HandleSavedFamilyHistory
            )

        HandleSavedFamilyHistory data ->
            ( { model | saveFamilyHistory = data }
            , Cmd.none
            )

        SaveOutsideCare personId valueId value ->
            ( { model | saveOutsideCare = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdOutsideCareEndpoint HandleSavedOutsideCare
            )

        HandleSavedOutsideCare data ->
            ( { model | saveOutsideCare = data }
            , Cmd.none
            )

        SaveHIVTest personId valueId value ->
            ( { model | saveHIVTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdHIVTestEndpoint HandleSavedHIVTest
            )

        HandleSavedHIVTest data ->
            ( { model | saveHIVTest = data }
            , Cmd.none
            )

        SaveUrineDipstickTest personId valueId value ->
            ( { model | saveUrineDipstickTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdUrineDipstickTestEndpoint HandleSavedUrineDipstickTest
            )

        HandleSavedUrineDipstickTest data ->
            ( { model | saveUrineDipstickTest = data }
            , Cmd.none
            )

        SaveRandomBloodSugarTest personId valueId value ->
            ( { model | saveRandomBloodSugarTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdRandomBloodSugarTestEndpoint HandleSavedRandomBloodSugarTest
            )

        HandleSavedRandomBloodSugarTest data ->
            ( { model | saveRandomBloodSugarTest = data }
            , Cmd.none
            )

        SaveLabsResults personId valueId value ->
            ( { model | saveLabsResults = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdLabsResultsEndpoint HandleSavedLabsResults
            )

        HandleSavedLabsResults data ->
            ( { model | saveLabsResults = data }
            , Cmd.none
            )

        SavePregnancyTest personId valueId value ->
            ( { model | savePregnancyTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdPregnancyTestEndpoint HandleSavedPregnancyTest
            )

        HandleSavedPregnancyTest data ->
            ( { model | savePregnancyTest = data }
            , Cmd.none
            )

        SaveCreatinineTest personId valueId value ->
            ( { model | saveCreatinineTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdCreatinineTestEndpoint HandleSavedCreatinineTest
            )

        HandleSavedCreatinineTest data ->
            ( { model | saveCreatinineTest = data }
            , Cmd.none
            )

        SaveLiverFunctionTest personId valueId value ->
            ( { model | saveLiverFunctionTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdLiverFunctionTestEndpoint HandleSavedLiverFunctionTest
            )

        HandleSavedLiverFunctionTest data ->
            ( { model | saveLiverFunctionTest = data }
            , Cmd.none
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdHealthEducationEndpoint HandleSavedHealthEducation
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            )

        SaveMedicationDistribution personId valueId value ->
            ( { model | saveMedicationDistribution = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdMedicationDistributionEndpoint HandleSavedMedicationDistribution
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            )

        SaveReferral personId valueId value ->
            ( { model | saveReferral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ncdReferralEndpoint HandleSavedReferral
            )

        HandleSavedReferral data ->
            ( { model | saveReferral = data }
            , Cmd.none
            )
