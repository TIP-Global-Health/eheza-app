module Backend.PrenatalEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.PrenatalEncounter.Encoder exposing (encodePrenatalEncounter)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> PrenatalEncounterId -> Maybe PrenatalEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        ClosePrenatalEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closePrenatalEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull prenatalEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedPrenatalEncounter)
                        )
                    )

        HandleClosedPrenatalEncounter data ->
            ( { model | closePrenatalEncounter = data }
            , Cmd.none
            )

        SaveBreastExam personId valueId value ->
            ( { model | saveBreastExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value breastExamEndpoint HandleSavedBreastExam
            )

        HandleSavedBreastExam data ->
            ( { model | saveBreastExam = data }
            , Cmd.none
            )

        SaveCorePhysicalExam personId valueId value ->
            ( { model | saveCorePhysicalExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value corePhysicalExamEndpoint HandleSavedCorePhysicalExam
            )

        HandleSavedCorePhysicalExam data ->
            ( { model | saveCorePhysicalExam = data }
            , Cmd.none
            )

        SaveDangerSigns personId valueId value ->
            ( { model | saveDangerSigns = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value dangerSignsEndpoint HandleSavedDangerSigns
            )

        HandleSavedDangerSigns data ->
            ( { model | saveDangerSigns = data }
            , Cmd.none
            )

        SaveLastMenstrualPeriod personId valueId value ->
            ( { model | saveLastMenstrualPeriod = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value lastMenstrualPeriodEndpoint HandleSavedLastMenstrualPeriod
            )

        HandleSavedLastMenstrualPeriod data ->
            ( { model | saveLastMenstrualPeriod = data }
            , Cmd.none
            )

        SaveMedicalHistory personId valueId value ->
            ( { model | saveMedicalHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value medicalHistoryEndpoint HandleSavedMedicalHistory
            )

        HandleSavedMedicalHistory data ->
            ( { model | saveMedicalHistory = data }
            , Cmd.none
            )

        SaveMedication personId valueId value ->
            ( { model | saveMedication = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value medicationEndpoint HandleSavedMedication
            )

        HandleSavedMedication data ->
            ( { model | saveMedication = data }
            , Cmd.none
            )

        SaveObstetricalExam personId valueId value ->
            ( { model | saveObstetricalExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value obstetricalExamEndpoint HandleSavedObstetricalExam
            )

        HandleSavedObstetricalExam data ->
            ( { model | saveObstetricalExam = data }
            , Cmd.none
            )

        SaveObstetricHistory personId valueId value ->
            ( { model | saveObstetricHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value obstetricHistoryEndpoint HandleSavedObstetricHistory
            )

        HandleSavedObstetricHistory data ->
            ( { model | saveObstetricHistory = data }
            , Cmd.none
            )

        SaveObstetricHistoryStep2 personId valueId value ->
            ( { model | saveObstetricHistoryStep2 = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value obstetricHistoryStep2Endpoint HandleSavedObstetricHistoryStep2
            )

        HandleSavedObstetricHistoryStep2 data ->
            ( { model | saveObstetricHistoryStep2 = data }
            , Cmd.none
            )

        SaveFamilyPlanning personId valueId value ->
            ( { model | saveFamilyPlanning = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalFamilyPlanningEndpoint HandleSavedFamilyPlanning
            )

        HandleSavedFamilyPlanning data ->
            ( { model | saveFamilyPlanning = data }
            , Cmd.none
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalNutritionEndpoint HandleSavedNutrition
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            )

        SaveResource personId valueId value ->
            ( { model | saveResource = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value resourceEndpoint HandleSavedResource
            )

        HandleSavedResource data ->
            ( { model | saveResource = data }
            , Cmd.none
            )

        SaveSocialHistory personId valueId value ->
            ( { model | saveSocialHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value socialHistoryEndpoint HandleSavedSocialHistory
            )

        HandleSavedSocialHistory data ->
            ( { model | saveSocialHistory = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value vitalsEndpoint HandleSavedVitals
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )

        SavePrenatalPhoto personId valueId value ->
            ( { model | savePrenatalPhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalPhotoEndpoint HandleSavedPrenatalPhoto
            )

        HandleSavedPrenatalPhoto data ->
            ( { model | savePrenatalPhoto = data }
            , Cmd.none
            )

        SaveBirthPlan personId valueId value ->
            ( { model | saveBirthPlan = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value birthPlanEndpoint HandleSavedBirthPlan
            )

        HandleSavedBirthPlan data ->
            ( { model | savePrenatalPhoto = data }
            , Cmd.none
            )

        SavePregnancyTesting personId valueId value ->
            ( { model | savePregnancyTesting = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value pregnancyTestingEndpoint HandleSavedPregnancyTesting
            )

        HandleSavedPregnancyTesting data ->
            ( { model | savePregnancyTesting = data }
            , Cmd.none
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalHealthEducationEndpoint HandleSavedHealthEducation
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalFollowUpEndpoint HandleSavedFollowup
            )

        HandleSavedFollowup data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalSendToHcEndpoint HandleSavedSendToHC
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            )

        SaveAppointmentConfirmation personId valueId value ->
            ( { model | saveAppointmentConfirmation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value appointmentConfirmationEndpoint HandleSavedAppointmentConfirmation
            )

        HandleSavedAppointmentConfirmation data ->
            ( { model | saveAppointmentConfirmation = data }
            , Cmd.none
            )
