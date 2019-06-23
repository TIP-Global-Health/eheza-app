module Backend.Session.Update exposing (update)

import AssocList as Dict
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Session.Encoder exposing (..)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
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
            , object [ encodeClosed True ]
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
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveHeightRequest = Dict.insert childId Loading model.saveHeightRequest }
                    , cmd
                    )

                SaveWeight maybeId weight ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveWeightRequest = Dict.insert childId Loading model.saveWeightRequest }
                    , cmd
                    )

                SaveMuac maybeId muac ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveMuacRequest = Dict.insert childId Loading model.saveMuacRequest }
                    , cmd
                    )

                SaveCounselingSession maybeId timing topics ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveCounselingSessionRequest = Dict.insert childId Loading model.saveCounselingSessionRequest }
                    , cmd
                    )

                SaveChildNutritionSigns maybeId signs ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveNutritionRequest = Dict.insert childId Loading model.saveNutritionRequest }
                    , cmd
                    )

                SavePhoto maybeId photo ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | savePhotoRequest = Dict.insert childId Loading model.savePhotoRequest }
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
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveAttendanceRequest = Dict.insert motherId Loading model.saveAttendanceRequest }
                    , cmd
                    )

                SaveFamilyPlanningSigns maybeId signs ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = motherId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveFamilyPlanningRequest = Dict.insert motherId Loading model.saveFamilyPlanningRequest }
                    , cmd
                    )

                SaveCompletedForm maybeId formId language ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = motherId
                                    , dateMeasured = currentDate
                                    , sessionId = Just sessionId
                                    , nurse = nurseId
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
                    ( { model | saveParticipantConsentRequest = Dict.insert motherId Loading model.saveParticipantConsentRequest }
                    , cmd
                    )

        HandleSaveAttendance motherId data ->
            ( { model | saveAttendanceRequest = Dict.insert motherId data model.saveAttendanceRequest }
            , Cmd.none
            )

        HandleSaveCounselingSession childId data ->
            ( { model | saveCounselingSessionRequest = Dict.insert childId data model.saveCounselingSessionRequest }
            , Cmd.none
            )

        HandleSaveParticipantConsent motherId data ->
            ( { model | saveParticipantConsentRequest = Dict.insert motherId data model.saveParticipantConsentRequest }
            , Cmd.none
            )

        HandleSaveFamilyPlanning motherId data ->
            ( { model | saveFamilyPlanningRequest = Dict.insert motherId data model.saveFamilyPlanningRequest }
            , Cmd.none
            )

        HandleSaveHeight childId data ->
            ( { model | saveHeightRequest = Dict.insert childId data model.saveHeightRequest }
            , Cmd.none
            )

        HandleSaveWeight childId data ->
            ( { model | saveWeightRequest = Dict.insert childId data model.saveWeightRequest }
            , Cmd.none
            )

        HandleSaveMuac childId data ->
            ( { model | saveMuacRequest = Dict.insert childId data model.saveMuacRequest }
            , Cmd.none
            )

        HandleSavePhoto childId data ->
            ( { model | savePhotoRequest = Dict.insert childId data model.savePhotoRequest }
            , Cmd.none
            )

        HandleSaveNutrition childId data ->
            ( { model | saveNutritionRequest = Dict.insert childId data model.saveNutritionRequest }
            , Cmd.none
            )
