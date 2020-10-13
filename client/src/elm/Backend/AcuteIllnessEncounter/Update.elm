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
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | endDate = Just currentDate }) model

        SetAcuteIllnessDiagnosis diagnosis ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | diagnosis = diagnosis }) model

        HandleUpdatedAcuteIllnessEncounter data ->
            ( { model | updateAcuteIllnessEncounter = data }
            , Cmd.none
            )

        SaveSymptomsGeneral personId valueId value ->
            ( { model | saveSymptomsGeneral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value symptomsGeneralEndpoint HandleSavedSymptomsGeneral
            )

        HandleSavedSymptomsGeneral data ->
            ( { model | saveSymptomsGeneral = data }
            , Cmd.none
            )

        SaveSymptomsRespiratory personId valueId value ->
            ( { model | saveSymptomsRespiratory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value symptomsRespiratoryEndpoint HandleSavedSymptomsRespiratory
            )

        HandleSavedSymptomsRespiratory data ->
            ( { model | saveSymptomsRespiratory = data }
            , Cmd.none
            )

        SaveSymptomsGI personId valueId value ->
            ( { model | saveSymptomsGI = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value symptomsGIEndpoint HandleSavedSymptomsGI
            )

        HandleSavedSymptomsGI data ->
            ( { model | saveSymptomsGI = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteIllnessVitalsEndpoint HandleSavedVitals
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )

        SaveAcuteFindings personId valueId value ->
            ( { model | saveAcuteFindings = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value acuteFindingsEndpoint HandleSavedAcuteFindings
            )

        HandleSavedAcuteFindings data ->
            ( { model | saveAcuteFindings = data }
            , Cmd.none
            )

        SaveMalariaTesting personId valueId value ->
            ( { model | saveMalariaTesting = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value malariaTestingEndpoint HandleSavedMalariaTesting
            )

        HandleSavedMalariaTesting data ->
            ( { model | saveMalariaTesting = data }
            , Cmd.none
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value sendToHCEndpoint HandleSavedSendToHC
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            )

        SaveMedicationDistribution personId valueId value ->
            ( { model | saveMedicationDistribution = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value medicationDistributionEndpoint HandleSavedMedicationDistribution
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            )

        SaveTravelHistory personId valueId value ->
            ( { model | saveTravelHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value travelHistoryEndpoint HandleSavedTravelHistory
            )

        HandleSavedTravelHistory data ->
            ( { model | saveTravelHistory = data }
            , Cmd.none
            )

        SaveExposure personId valueId value ->
            ( { model | saveExposure = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value exposureEndpoint HandleSavedExposure
            )

        HandleSavedExposure data ->
            ( { model | saveExposure = data }
            , Cmd.none
            )

        SaveIsolation personId valueId value ->
            ( { model | saveIsolation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value isolationEndpoint HandleSavedIsolation
            )

        HandleSavedIsolation data ->
            ( { model | saveIsolation = data }
            , Cmd.none
            )

        SaveHCContact personId valueId value ->
            ( { model | saveHCContact = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hcContactEndpoint HandleSavedHCContact
            )

        HandleSavedHCContact data ->
            ( { model | saveHCContact = data }
            , Cmd.none
            )

        SaveCall114 personId valueId value ->
            let
                cmd =
                    case valueId of
                        Nothing ->
                            { participantId = personId
                            , dateMeasured = currentDate
                            , encounterId = Just encounterId
                            , nurse = nurseId
                            , healthCenter = healthCenterId
                            , value = value
                            }
                                |> sw.post call114Endpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedCall114)

                        Just id ->
                            encodeCall114Value value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny call114Endpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedCall114)
            in
            ( { model | saveCall114 = Loading }
            , cmd
            )

        HandleSavedCall114 data ->
            ( { model | saveCall114 = data }
            , Cmd.none
            )

        SaveTreatmentReview personId valueId value ->
            ( { model | saveTreatmentReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value treatmentReviewEndpoint HandleSavedTreatmentReview
            )

        HandleSavedTreatmentReview data ->
            ( { model | saveTreatmentReview = data }
            , Cmd.none
            )


updateEncounter : NominalDate -> AcuteIllnessEncounterId -> Maybe AcuteIllnessEncounter -> (AcuteIllnessEncounter -> AcuteIllnessEncounter) -> Model -> ( Model, Cmd Msg )
updateEncounter currentDate encounterId maybeEncounter updateFunc model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    maybeEncounter
        |> unwrap ( model, Cmd.none )
            (\encounter ->
                ( { model | updateAcuteIllnessEncounter = Loading }
                , updateFunc encounter
                    |> sw.patchFull acuteIllnessEncounterEndpoint encounterId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedAcuteIllnessEncounter)
                )
            )
