module Backend.AcuteIllnessEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter, Model, Msg(..))
import Backend.Endpoints exposing (acuteFindingsEndpoint, acuteIllnessContactsTracingEndpoint, acuteIllnessCoreExamEndpoint, acuteIllnessDangerSignsEndpoint, acuteIllnessEncounterEndpoint, acuteIllnessFollowUpEndpoint, acuteIllnessMuacEndpoint, acuteIllnessNutritionEndpoint, acuteIllnessTraceContactEndpoint, acuteIllnessVitalsEndpoint, call114Endpoint, covidTestingEndpoint, exposureEndpoint, hcContactEndpoint, healthEducationEndpoint, isolationEndpoint, malariaTestingEndpoint, medicationDistributionEndpoint, sendToHCEndpoint, symptomsGIEndpoint, symptomsGeneralEndpoint, symptomsRespiratoryEndpoint, travelHistoryEndpoint, treatmentOngoingEndpoint, treatmentReviewEndpoint)
import Backend.Entities exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> AcuteIllnessEncounterId
    -> Maybe AcuteIllnessEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseAcuteIllnessEncounter ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | endDate = Just currentDate }) model

        SetAcuteIllnessDiagnosis diagnosis ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | diagnosis = diagnosis }) model

        HandleUpdatedAcuteIllnessEncounter data ->
            ( { model | updateAcuteIllnessEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomsGeneral personId valueId value ->
            ( { model | saveSymptomsGeneral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value symptomsGeneralEndpoint HandleSavedSymptomsGeneral
            , []
            )

        HandleSavedSymptomsGeneral data ->
            ( { model | saveSymptomsGeneral = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomsRespiratory personId valueId value ->
            ( { model | saveSymptomsRespiratory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value symptomsRespiratoryEndpoint HandleSavedSymptomsRespiratory
            , []
            )

        HandleSavedSymptomsRespiratory data ->
            ( { model | saveSymptomsRespiratory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomsGI personId valueId value ->
            ( { model | saveSymptomsGI = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value symptomsGIEndpoint HandleSavedSymptomsGI
            , []
            )

        HandleSavedSymptomsGI data ->
            ( { model | saveSymptomsGI = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessVitalsEndpoint HandleSavedVitals
            , []
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveAcuteFindings personId valueId value ->
            ( { model | saveAcuteFindings = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteFindingsEndpoint HandleSavedAcuteFindings
            , []
            )

        HandleSavedAcuteFindings data ->
            ( { model | saveAcuteFindings = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMalariaTesting personId valueId value ->
            ( { model | saveMalariaTesting = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value malariaTestingEndpoint HandleSavedMalariaTesting
            , []
            )

        HandleSavedMalariaTesting data ->
            ( { model | saveMalariaTesting = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCovidTesting personId valueId value ->
            ( { model | saveCovidTesting = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value covidTestingEndpoint HandleSavedCovidTesting
            , []
            )

        HandleSavedCovidTesting data ->
            ( { model | saveCovidTesting = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value sendToHCEndpoint HandleSavedSendToHC
            , []
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMedicationDistribution personId valueId value ->
            ( { model | saveMedicationDistribution = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value medicationDistributionEndpoint HandleSavedMedicationDistribution
            , []
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveTravelHistory personId valueId value ->
            ( { model | saveTravelHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value travelHistoryEndpoint HandleSavedTravelHistory
            , []
            )

        HandleSavedTravelHistory data ->
            ( { model | saveTravelHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveExposure personId valueId value ->
            ( { model | saveExposure = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value exposureEndpoint HandleSavedExposure
            , []
            )

        HandleSavedExposure data ->
            ( { model | saveExposure = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveIsolation personId valueId value ->
            ( { model | saveIsolation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value isolationEndpoint HandleSavedIsolation
            , []
            )

        HandleSavedIsolation data ->
            ( { model | saveIsolation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHCContact personId valueId value ->
            ( { model | saveHCContact = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hcContactEndpoint HandleSavedHCContact
            , []
            )

        HandleSavedHCContact data ->
            ( { model | saveHCContact = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCall114 personId valueId value ->
            ( { model | saveCall114 = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value call114Endpoint HandleSavedCall114
            , []
            )

        HandleSavedCall114 data ->
            ( { model | saveCall114 = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveTreatmentReview personId valueId value ->
            ( { model | saveTreatmentReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value treatmentReviewEndpoint HandleSavedTreatmentReview
            , []
            )

        HandleSavedTreatmentReview data ->
            ( { model | saveTreatmentReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessMuacEndpoint HandleSavedMuac
            , []
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveTreatmentOngoing personId valueId value ->
            ( { model | saveTreatmentOngoing = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value treatmentOngoingEndpoint HandleSavedTreatmentOngoing
            , []
            )

        HandleSavedTreatmentOngoing data ->
            ( { model | saveTreatmentOngoing = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDangerSigns personId valueId value ->
            ( { model | saveDangerSigns = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessDangerSignsEndpoint HandleSavedDangerSigns
            , []
            )

        HandleSavedDangerSigns data ->
            ( { model | saveDangerSigns = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessNutritionEndpoint HandleSavedNutrition
            , []
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value healthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessFollowUpEndpoint HandleSavedFollowUp
            , []
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCoreExam personId valueId value ->
            ( { model | saveCoreExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessCoreExamEndpoint HandleSavedCoreExam
            , []
            )

        HandleSavedCoreExam data ->
            ( { model | saveCoreExam = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveContactsTracing personId valueId value ->
            ( { model | saveContactsTracing = Loading }
            , saveMeasurementCmd currentDate
                encounterId
                personId
                nurseId
                healthCenterId
                valueId
                value
                acuteIllnessContactsTracingEndpoint
                (HandleSavedContactsTracing personId value)
            , []
            )

        HandleSavedContactsTracing personId items data ->
            let
                createTraceContactMsgs =
                    case data of
                        Success () ->
                            List.map (SaveTraceContact personId Nothing) items

                        _ ->
                            []
            in
            ( { model | saveContactsTracing = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
                |> sequenceExtra (update currentDate nurseId healthCenterId encounterId maybeEncounter) createTraceContactMsgs

        SaveTraceContact personId valueId value ->
            ( { model | saveTraceContact = Dict.insert value.personId Loading model.saveTraceContact }
            , saveMeasurementCmd currentDate
                encounterId
                personId
                nurseId
                healthCenterId
                valueId
                value
                acuteIllnessTraceContactEndpoint
                (HandleSavedTraceContact value.personId)
            , []
            )

        HandleSavedTraceContact tracedPersonId data ->
            ( { model | saveTraceContact = Dict.insert tracedPersonId data model.saveTraceContact }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateEncounter :
    NominalDate
    -> AcuteIllnessEncounterId
    -> Maybe AcuteIllnessEncounter
    -> (AcuteIllnessEncounter -> AcuteIllnessEncounter)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateEncounter currentDate encounterId maybeEncounter updateFunc model =
    maybeEncounter
        |> unwrap ( model, Cmd.none, [] )
            (\encounter ->
                ( { model | updateAcuteIllnessEncounter = Loading }
                , updateFunc encounter
                    |> sw.patchFull acuteIllnessEncounterEndpoint encounterId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedAcuteIllnessEncounter)
                , []
                )
            )
