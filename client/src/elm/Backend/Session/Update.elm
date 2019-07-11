module Backend.Session.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Session.Model exposing (..)
import EveryDict
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> SessionId -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId sessionId currentDate msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        CloseSession ->
            ( { model | closeSessionRequest = Loading }
            , object [ ( "scheduled_date.value2", encodeYYYYMMDD currentDate ) ]
                |> sw.patchAny sessionEndpoint sessionId
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleClosedSession)
            )

        HandleClosedSession data ->
            ( { model | closeSessionRequest = data }
            , Cmd.none
            )

        MeasurementOutMsgChild childId subMsg ->
            case subMsg of
                SaveHeight maybeId height ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = height
                                    }
                                        |> sw.post heightEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveHeight childId)

                                Just id ->
                                    encodeHeightValue height
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny heightEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveHeight childId)
                    in
                    ( { model | saveHeightRequest = EveryDict.insert childId Loading model.saveHeightRequest }
                    , cmd
                    )

                SaveWeight maybeId weight ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = weight
                                    }
                                        |> sw.post weightEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveWeight childId)

                                Just id ->
                                    encodeWeightValue weight
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny weightEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveWeight childId)
                    in
                    ( { model | saveWeightRequest = EveryDict.insert childId Loading model.saveWeightRequest }
                    , cmd
                    )

                SaveMuac maybeId muac ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = muac
                                    }
                                        |> sw.post muacEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveMuac childId)

                                Just id ->
                                    encodeMuacValue muac
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny muacEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveMuac childId)
                    in
                    ( { model | saveMuacRequest = EveryDict.insert childId Loading model.saveMuacRequest }
                    , cmd
                    )

                SaveCounselingSession maybeId timing topics ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = ( timing, topics )
                                    }
                                        |> sw.post counselingSessionEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveCounselingSession childId)

                                Just id ->
                                    ( timing, topics )
                                        |> encodeCounselingSessionValue
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny counselingSessionEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveCounselingSession childId)
                    in
                    ( { model | saveCounselingSessionRequest = EveryDict.insert childId Loading model.saveCounselingSessionRequest }
                    , cmd
                    )

                SaveChildNutritionSigns maybeId signs ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = signs
                                    }
                                        |> sw.post nutritionEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveNutrition childId)

                                Just id ->
                                    encodeNutritionValue signs
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny nutritionEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveNutrition childId)
                    in
                    ( { model | saveNutritionRequest = EveryDict.insert childId Loading model.saveNutritionRequest }
                    , cmd
                    )

                SavePhoto maybeId photo ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = photo
                                    }
                                        |> sw.post photoEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSavePhoto childId)

                                Just id ->
                                    encodePhotoUrl photo
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny photoEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSavePhoto childId)
                    in
                    ( { model | savePhotoRequest = EveryDict.insert childId Loading model.savePhotoRequest }
                    , cmd
                    )

        -- We're handling responses in order to pick up error conditions.
        -- However, we'll let the general "handleRevision" mechanism handle
        -- integrating the data into our model ... we need that anyway, so
        -- there's no point having two separate code paths to do it.
        MeasurementOutMsgMother motherId subMsg ->
            case subMsg of
                SaveAttendance maybeId attended ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = motherId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = attended
                                    }
                                        |> sw.post attendanceEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveAttendance motherId)

                                Just id ->
                                    encodeAttendanceValue attended
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny attendanceEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveAttendance motherId)
                    in
                    ( { model | saveAttendanceRequest = EveryDict.insert motherId Loading model.saveAttendanceRequest }
                    , cmd
                    )

                SaveFamilyPlanningSigns maybeId signs ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = motherId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = signs
                                    }
                                        |> sw.post familyPlanningEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveFamilyPlanning motherId)

                                Just id ->
                                    encodeFamilyPlanningValue signs
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny familyPlanningEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveFamilyPlanning motherId)
                    in
                    ( { model | saveFamilyPlanningRequest = EveryDict.insert motherId Loading model.saveFamilyPlanningRequest }
                    , cmd
                    )

                SaveCompletedForm maybeId formId language ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = motherId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value =
                                        { language = language
                                        , formId = formId
                                        }
                                    }
                                        |> sw.post participantConsentEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveParticipantConsent motherId)

                                Just id ->
                                    { formId = formId
                                    , language = language
                                    }
                                        |> encodeParticipantConsentValue
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny participantConsentEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveParticipantConsent motherId)
                    in
                    ( { model | saveParticipantConsentRequest = EveryDict.insert motherId Loading model.saveParticipantConsentRequest }
                    , cmd
                    )

        HandleSaveAttendance motherId data ->
            ( { model | saveAttendanceRequest = EveryDict.insert motherId data model.saveAttendanceRequest }
            , Cmd.none
            )

        HandleSaveCounselingSession childId data ->
            ( { model | saveCounselingSessionRequest = EveryDict.insert childId data model.saveCounselingSessionRequest }
            , Cmd.none
            )

        HandleSaveParticipantConsent motherId data ->
            ( { model | saveParticipantConsentRequest = EveryDict.insert motherId data model.saveParticipantConsentRequest }
            , Cmd.none
            )

        HandleSaveFamilyPlanning motherId data ->
            ( { model | saveFamilyPlanningRequest = EveryDict.insert motherId data model.saveFamilyPlanningRequest }
            , Cmd.none
            )

        HandleSaveHeight childId data ->
            ( { model | saveHeightRequest = EveryDict.insert childId data model.saveHeightRequest }
            , Cmd.none
            )

        HandleSaveWeight childId data ->
            ( { model | saveWeightRequest = EveryDict.insert childId data model.saveWeightRequest }
            , Cmd.none
            )

        HandleSaveMuac childId data ->
            ( { model | saveMuacRequest = EveryDict.insert childId data model.saveMuacRequest }
            , Cmd.none
            )

        HandleSavePhoto childId data ->
            ( { model | savePhotoRequest = EveryDict.insert childId data model.savePhotoRequest }
            , Cmd.none
            )

        HandleSaveNutrition childId data ->
            ( { model | saveNutritionRequest = EveryDict.insert childId data model.saveNutritionRequest }
            , Cmd.none
            )
