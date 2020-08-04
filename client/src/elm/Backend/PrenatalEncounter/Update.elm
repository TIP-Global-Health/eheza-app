module Backend.PrenatalEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> PrenatalEncounterId -> Maybe PrenatalEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        ClosePrenatalEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closePrenatalEncounter = Loading }
                        , object
                            [ ( "scheduled_date"
                              , object
                                    [ ( "value", encodeYYYYMMDD encounter.startDate )
                                    , ( "value2", encodeYYYYMMDD currentDate )
                                    ]
                              )
                            ]
                            |> sw.patchAny prenatalEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedPrenatalEncounter)
                        )
                    )

        HandleClosedPrenatalEncounter data ->
            ( { model | closePrenatalEncounter = data }
            , Cmd.none
            )

        SaveBreastExam personId valueId value ->
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
                                |> sw.post breastExamEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedBreastExam)

                        Just id ->
                            encodeBreastExamValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny breastExamEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedBreastExam)
            in
            ( { model | saveBreastExam = Loading }
            , cmd
            )

        HandleSavedBreastExam data ->
            ( { model | saveBreastExam = data }
            , Cmd.none
            )

        SaveCorePhysicalExam personId valueId value ->
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
                                |> sw.post corePhysicalExamEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedCorePhysicalExam)

                        Just id ->
                            encodeCorePhysicalExamValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny corePhysicalExamEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedCorePhysicalExam)
            in
            ( { model | saveCorePhysicalExam = Loading }
            , cmd
            )

        HandleSavedCorePhysicalExam data ->
            ( { model | saveCorePhysicalExam = data }
            , Cmd.none
            )

        SaveDangerSigns personId valueId value ->
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
                                |> sw.post dangerSignsEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedDangerSigns)

                        Just id ->
                            encodeDangerSignsValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny dangerSignsEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedDangerSigns)
            in
            ( { model | saveDangerSigns = Loading }
            , cmd
            )

        HandleSavedDangerSigns data ->
            ( { model | saveDangerSigns = data }
            , Cmd.none
            )

        SaveLastMenstrualPeriod personId valueId value ->
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
                                |> sw.post lastMenstrualPeriodEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedLastMenstrualPeriod)

                        Just id ->
                            encodeLastMenstrualPeriodValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny lastMenstrualPeriodEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedLastMenstrualPeriod)
            in
            ( { model | saveLastMenstrualPeriod = Loading }
            , cmd
            )

        HandleSavedLastMenstrualPeriod data ->
            ( { model | saveLastMenstrualPeriod = data }
            , Cmd.none
            )

        SaveMedicalHistory personId valueId value ->
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
                                |> sw.post medicalHistoryEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMedicalHistory)

                        Just id ->
                            encodeMedicalHistoryValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny medicalHistoryEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMedicalHistory)
            in
            ( { model | saveMedicalHistory = Loading }
            , cmd
            )

        HandleSavedMedicalHistory data ->
            ( { model | saveMedicalHistory = data }
            , Cmd.none
            )

        SaveMedication personId valueId value ->
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
                                |> sw.post medicationEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMedication)

                        Just id ->
                            encodeMedicationValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny medicationEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMedication)
            in
            ( { model | saveMedication = Loading }
            , cmd
            )

        HandleSavedMedication data ->
            ( { model | saveMedication = data }
            , Cmd.none
            )

        SaveObstetricalExam personId valueId value ->
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
                                |> sw.post obstetricalExamEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedObstetricalExam)

                        Just id ->
                            encodeObstetricalExamValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny obstetricalExamEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedObstetricalExam)
            in
            ( { model | saveObstetricalExam = Loading }
            , cmd
            )

        HandleSavedObstetricalExam data ->
            ( { model | saveObstetricalExam = data }
            , Cmd.none
            )

        SaveObstetricHistory personId valueId value ->
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
                                |> sw.post obstetricHistoryEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedObstetricHistory)

                        Just id ->
                            encodeObstetricHistoryValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny obstetricHistoryEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedObstetricHistory)
            in
            ( { model | saveObstetricHistory = Loading }
            , cmd
            )

        HandleSavedObstetricHistory data ->
            ( { model | saveObstetricHistory = data }
            , Cmd.none
            )

        SaveObstetricHistoryStep2 personId valueId value ->
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
                                |> sw.post obstetricHistoryStep2Endpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedObstetricHistoryStep2)

                        Just id ->
                            encodeObstetricHistoryStep2Value value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny obstetricHistoryStep2Endpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedObstetricHistoryStep2)
            in
            ( { model | saveObstetricHistoryStep2 = Loading }
            , cmd
            )

        HandleSavedObstetricHistoryStep2 data ->
            ( { model | saveObstetricHistoryStep2 = data }
            , Cmd.none
            )

        SaveFamilyPlanning personId valueId value ->
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
                                |> sw.post prenatalFamilyPlanningEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedFamilyPlanning)

                        Just id ->
                            encodePrenatalFamilyPlanningValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny prenatalFamilyPlanningEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedFamilyPlanning)
            in
            ( { model | saveFamilyPlanning = Loading }
            , cmd
            )

        HandleSavedFamilyPlanning data ->
            ( { model | saveFamilyPlanning = data }
            , Cmd.none
            )

        SaveNutrition personId valueId value ->
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
                                |> sw.post prenatalNutritionEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedNutrition)

                        Just id ->
                            encodePrenatalNutritionValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny prenatalNutritionEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedNutrition)
            in
            ( { model | saveNutrition = Loading }
            , cmd
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            )

        SaveResource personId valueId value ->
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
                                |> sw.post resourceEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedResource)

                        Just id ->
                            encodeResourceValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny resourceEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedResource)
            in
            ( { model | saveResource = Loading }
            , cmd
            )

        HandleSavedResource data ->
            ( { model | saveResource = data }
            , Cmd.none
            )

        SaveSocialHistory personId valueId value ->
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
                                |> sw.post socialHistoryEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSocialHistory)

                        Just id ->
                            encodeSocialHistoryValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny socialHistoryEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSocialHistory)
            in
            ( { model | saveSocialHistory = Loading }
            , cmd
            )

        HandleSavedSocialHistory data ->
            ( { model | saveSocialHistory = data }
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
                                |> sw.post vitalsEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedVitals)

                        Just id ->
                            encodeVitalsValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny vitalsEndpoint id
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

        SavePrenatalPhoto personId valueId value ->
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
                                |> sw.post prenatalPhotoEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedPrenatalPhoto)

                        Just id ->
                            encodePhotoUrl value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny prenatalPhotoEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedPrenatalPhoto)
            in
            ( { model | savePrenatalPhoto = Loading }
            , cmd
            )

        HandleSavedPrenatalPhoto data ->
            ( { model | savePrenatalPhoto = data }
            , Cmd.none
            )
