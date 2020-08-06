module Backend.AcuteIllnessEncounter.Update exposing (update)

import Backend.AcuteIllnessEncounter.Encoder exposing (encodeAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> AcuteIllnessEncounterId -> Maybe AcuteIllnessEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseAcuteIllnessEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeAcuteIllnessEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> encodeAcuteIllnessEncounter
                            |> object
                            |> sw.patchAny acuteIllnessEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedAcuteIllnessEncounter)
                        )
                    )

        HandleClosedAcuteIllnessEncounter data ->
            ( { model | closeAcuteIllnessEncounter = data }
            , Cmd.none
            )

        SaveSymptomsGeneral personId valueId value ->
            ( { model | saveSymptomsGeneral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeSymptomsGeneral symptomsGeneralEndpoint HandleSavedSymptomsGeneral
            )

        HandleSavedSymptomsGeneral data ->
            ( { model | saveSymptomsGeneral = data }
            , Cmd.none
            )

        SaveSymptomsRespiratory personId valueId value ->
            ( { model | saveSymptomsRespiratory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeSymptomsRespiratory symptomsRespiratoryEndpoint HandleSavedSymptomsRespiratory
            )

        HandleSavedSymptomsRespiratory data ->
            ( { model | saveSymptomsRespiratory = data }
            , Cmd.none
            )

        SaveSymptomsGI personId valueId value ->
            ( { model | saveSymptomsGI = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeSymptomsGI symptomsGIEndpoint HandleSavedSymptomsGI
            )

        HandleSavedSymptomsGI data ->
            ( { model | saveSymptomsGI = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeAcuteIllnessVitals acuteIllnessVitalsEndpoint HandleSavedVitals
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )

        SaveAcuteFindings personId valueId value ->
            ( { model | saveAcuteFindings = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeAcuteFindings acuteFindingsEndpoint HandleSavedAcuteFindings
            )

        HandleSavedAcuteFindings data ->
            ( { model | saveAcuteFindings = data }
            , Cmd.none
            )

        SaveMalariaTesting personId valueId value ->
            ( { model | saveMalariaTesting = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeMalariaTesting malariaTestingEndpoint HandleSavedMalariaTesting
            )

        HandleSavedMalariaTesting data ->
            ( { model | saveMalariaTesting = data }
            , Cmd.none
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeSendToHC sendToHCEndpoint HandleSavedSendToHC
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            )

        SaveMedicationDistribution personId valueId value ->
            ( { model | saveMedicationDistribution = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeMedicationDistribution medicationDistributionEndpoint HandleSavedMedicationDistribution
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            )

        SaveTravelHistory personId valueId value ->
            ( { model | saveTravelHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeTravelHistory travelHistoryEndpoint HandleSavedTravelHistory
            )

        HandleSavedTravelHistory data ->
            ( { model | saveTravelHistory = data }
            , Cmd.none
            )

        SaveExposure personId valueId value ->
            ( { model | saveExposure = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeExposure exposureEndpoint HandleSavedExposure
            )

        HandleSavedExposure data ->
            ( { model | saveExposure = data }
            , Cmd.none
            )

        SaveIsolation personId valueId value ->
            ( { model | saveIsolation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeIsolation isolationEndpoint HandleSavedIsolation
            )

        HandleSavedIsolation data ->
            ( { model | saveIsolation = data }
            , Cmd.none
            )

        SaveHCContact personId valueId value ->
            ( { model | saveHCContact = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeHCContact hcContactEndpoint HandleSavedHCContact
            )

        HandleSavedHCContact data ->
            ( { model | saveHCContact = data }
            , Cmd.none
            )

        SaveTreatmentReview personId valueId value ->
            ( { model | saveTreatmentReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value encodeTreatmentReview treatmentReviewEndpoint HandleSavedTreatmentReview
            )

        HandleSavedTreatmentReview data ->
            ( { model | saveTreatmentReview = data }
            , Cmd.none
            )
