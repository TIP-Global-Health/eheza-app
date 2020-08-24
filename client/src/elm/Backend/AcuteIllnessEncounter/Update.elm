module Backend.AcuteIllnessEncounter.Update exposing (update)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> AcuteIllnessEncounterId -> Maybe AcuteIllnessEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
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
                                |> sw.post symptomsGeneralEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGeneral)

                        Just id ->
                            encodeSymptomsGeneralValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny symptomsGeneralEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGeneral)
            in
            ( { model | saveSymptomsGeneral = Loading }
            , cmd
            )

        HandleSavedSymptomsGeneral data ->
            ( { model | saveSymptomsGeneral = data }
            , Cmd.none
            )

        SaveSymptomsRespiratory personId valueId value ->
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
                                |> sw.post symptomsRespiratoryEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsRespiratory)

                        Just id ->
                            encodeSymptomsRespiratoryValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny symptomsRespiratoryEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsRespiratory)
            in
            ( { model | saveSymptomsRespiratory = Loading }
            , cmd
            )

        HandleSavedSymptomsRespiratory data ->
            ( { model | saveSymptomsRespiratory = data }
            , Cmd.none
            )

        SaveSymptomsGI personId valueId value ->
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
                                |> sw.post symptomsGIEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGI)

                        Just id ->
                            encodeSymptomsGIValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny symptomsGIEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGI)
            in
            ( { model | saveSymptomsGI = Loading }
            , cmd
            )

        HandleSavedSymptomsGI data ->
            ( { model | saveSymptomsGI = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
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
                                |> sw.post acuteIllnessVitalsEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedVitals)

                        Just id ->
                            encodeAcuteIllnessVitalsValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny acuteIllnessVitalsEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedVitals)
            in
            ( { model | saveVitals = Loading }
            , cmd
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )

        SaveAcuteFindings personId valueId value ->
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
                                |> sw.post acuteFindingsEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedAcuteFindings)

                        Just id ->
                            encodeAcuteFindingsValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny acuteFindingsEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedAcuteFindings)
            in
            ( { model | saveAcuteFindings = Loading }
            , cmd
            )

        HandleSavedAcuteFindings data ->
            ( { model | saveAcuteFindings = data }
            , Cmd.none
            )

        SaveMalariaTesting personId valueId value ->
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
                                |> sw.post malariaTestingEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMalariaTesting)

                        Just id ->
                            encodeMalariaTestingValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny malariaTestingEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMalariaTesting)
            in
            ( { model | saveMalariaTesting = Loading }
            , cmd
            )

        HandleSavedMalariaTesting data ->
            ( { model | saveMalariaTesting = data }
            , Cmd.none
            )

        SaveSendToHC personId valueId value ->
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
                                |> sw.post sendToHCEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSendToHC)

                        Just id ->
                            encodeSendToHCValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny sendToHCEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSendToHC)
            in
            ( { model | saveSendToHC = Loading }
            , cmd
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            )

        SaveMedicationDistribution personId valueId value ->
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
                                |> sw.post medicationDistributionEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMedicationDistribution)

                        Just id ->
                            encodeMedicationDistributionValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny medicationDistributionEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMedicationDistribution)
            in
            ( { model | saveMedicationDistribution = Loading }
            , cmd
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            )

        SaveTravelHistory personId valueId value ->
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
                                |> sw.post travelHistoryEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedTravelHistory)

                        Just id ->
                            encodeTravelHistoryValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny travelHistoryEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedTravelHistory)
            in
            ( { model | saveTravelHistory = Loading }
            , cmd
            )

        HandleSavedTravelHistory data ->
            ( { model | saveTravelHistory = data }
            , Cmd.none
            )

        SaveExposure personId valueId value ->
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
                                |> sw.post exposureEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedExposure)

                        Just id ->
                            encodeExposureValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny exposureEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedExposure)
            in
            ( { model | saveExposure = Loading }
            , cmd
            )

        HandleSavedExposure data ->
            ( { model | saveExposure = data }
            , Cmd.none
            )

        SaveIsolation personId valueId value ->
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
                                |> sw.post isolationEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedIsolation)

                        Just id ->
                            encodeIsolationValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny isolationEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedIsolation)
            in
            ( { model | saveIsolation = Loading }
            , cmd
            )

        HandleSavedIsolation data ->
            ( { model | saveIsolation = data }
            , Cmd.none
            )

        SaveHCContact personId valueId value ->
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
                                |> sw.post hcContactEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedHCContact)

                        Just id ->
                            encodeHCContactValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny hcContactEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedHCContact)
            in
            ( { model | saveHCContact = Loading }
            , cmd
            )

        HandleSavedHCContact data ->
            ( { model | saveHCContact = data }
            , Cmd.none
            )

        SaveTreatmentReview personId valueId value ->
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
                                |> sw.post treatmentReviewEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedTreatmentReview)

                        Just id ->
                            encodeTreatmentReviewValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny treatmentReviewEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedTreatmentReview)
            in
            ( { model | saveTreatmentReview = Loading }
            , cmd
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
