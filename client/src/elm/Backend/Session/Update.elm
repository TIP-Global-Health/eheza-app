module Backend.Session.Update exposing (update)

import App.Ports exposing (bindDropZone)
import AssocList as Dict
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> SessionId -> Maybe Session -> NominalDate -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List MsgIndexedDb )
update nurseId sessionId maybeSession currentDate db msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        CloseSession ->
            unwrap
                ( model, Cmd.none, [] )
                (\session ->
                    ( { model | closeSessionRequest = Loading }
                    , object
                        [ ( "scheduled_date"
                          , object
                                [ ( "value", encodeYYYYMMDD session.startDate )
                                , ( "value2", encodeYYYYMMDD currentDate )
                                ]
                          )
                        ]
                        |> sw.patchAny sessionEndpoint sessionId
                        |> withoutDecoder
                        |> toCmd (RemoteData.fromResult >> HandleClosedSession)
                    , []
                    )
                )
                maybeSession

        HandleClosedSession data ->
            ( { model | closeSessionRequest = data }
            , Cmd.none
            , []
            )

        MeasurementOutMsgChild childId subMsg ->
            case subMsg of
                FetchIndividualNutritionData id ->
                    ( model, Cmd.none, Backend.NutritionEncounter.Fetch.fetchForChild id db )

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
                    ( { model | saveHeightRequest = Dict.insert childId Loading model.saveHeightRequest }
                    , cmd
                    , []
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
                    ( { model | saveWeightRequest = Dict.insert childId Loading model.saveWeightRequest }
                    , cmd
                    , []
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
                    ( { model | saveMuacRequest = Dict.insert childId Loading model.saveMuacRequest }
                    , cmd
                    , []
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
                    ( { model | saveCounselingSessionRequest = Dict.insert childId Loading model.saveCounselingSessionRequest }
                    , cmd
                    , []
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
                    ( { model | saveNutritionRequest = Dict.insert childId Loading model.saveNutritionRequest }
                    , cmd
                    , []
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
                    ( { model | savePhotoRequest = Dict.insert childId Loading model.savePhotoRequest }
                    , cmd
                    , []
                    )

                SaveChildFbf maybeId value ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = childId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = value
                                    }
                                        |> sw.post childFbfEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveFbf childId)

                                Just id ->
                                    encodeFbfValue value
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny childFbfEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveFbf childId)
                    in
                    ( { model | saveFbfRequest = Dict.insert childId Loading model.saveLactationRequest }
                    , cmd
                    , []
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
                    ( { model | saveAttendanceRequest = Dict.insert motherId Loading model.saveAttendanceRequest }
                    , cmd
                    , []
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
                    ( { model | saveFamilyPlanningRequest = Dict.insert motherId Loading model.saveFamilyPlanningRequest }
                    , cmd
                    , []
                    )

                SaveLactation maybeId signs ->
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
                                        |> sw.post lactationEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveLactation motherId)

                                Just id ->
                                    encodeLactationValue signs
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny lactationEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveLactation motherId)
                    in
                    ( { model | saveLactationRequest = Dict.insert motherId Loading model.saveLactationRequest }
                    , cmd
                    , []
                    )

                SaveMotherFbf maybeId value ->
                    let
                        cmd =
                            case maybeId of
                                Nothing ->
                                    { participantId = motherId
                                    , dateMeasured = currentDate
                                    , encounterId = Just sessionId
                                    , nurse = nurseId
                                    , healthCenter = Nothing
                                    , value = value
                                    }
                                        |> sw.post motherFbfEndpoint
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveFbf motherId)

                                Just id ->
                                    encodeFbfValue value
                                        |> (::) ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                        |> object
                                        |> sw.patchAny motherFbfEndpoint id
                                        |> withoutDecoder
                                        |> toCmd (RemoteData.fromResult >> HandleSaveFbf motherId)
                    in
                    ( { model | saveFbfRequest = Dict.insert motherId Loading model.saveLactationRequest }
                    , cmd
                    , []
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
                    ( { model | saveParticipantConsentRequest = Dict.insert motherId Loading model.saveParticipantConsentRequest }
                    , cmd
                    , []
                    )

        HandleSaveAttendance motherId data ->
            ( { model | saveAttendanceRequest = Dict.insert motherId data model.saveAttendanceRequest }
            , Cmd.none
            , []
            )

        HandleSaveCounselingSession childId data ->
            ( { model | saveCounselingSessionRequest = Dict.insert childId data model.saveCounselingSessionRequest }
            , Cmd.none
            , []
            )

        HandleSaveParticipantConsent motherId data ->
            ( { model | saveParticipantConsentRequest = Dict.insert motherId data model.saveParticipantConsentRequest }
            , Cmd.none
            , []
            )

        HandleSaveFamilyPlanning motherId data ->
            ( { model | saveFamilyPlanningRequest = Dict.insert motherId data model.saveFamilyPlanningRequest }
            , Cmd.none
            , []
            )

        HandleSaveLactation motherId data ->
            ( { model | saveLactationRequest = Dict.insert motherId data model.saveLactationRequest }
            , Cmd.none
            , []
            )

        HandleSaveFbf personId data ->
            ( { model | saveFbfRequest = Dict.insert personId data model.saveFbfRequest }
            , Cmd.none
            , []
            )

        HandleSaveHeight childId data ->
            ( { model | saveHeightRequest = Dict.insert childId data model.saveHeightRequest }
            , bindDropZone ()
            , []
            )

        HandleSaveWeight childId data ->
            ( { model | saveWeightRequest = Dict.insert childId data model.saveWeightRequest }
            , bindDropZone ()
            , []
            )

        HandleSaveMuac childId data ->
            ( { model | saveMuacRequest = Dict.insert childId data model.saveMuacRequest }
            , bindDropZone ()
            , []
            )

        HandleSavePhoto childId data ->
            ( { model | savePhotoRequest = Dict.insert childId data model.savePhotoRequest }
            , bindDropZone ()
            , []
            )

        HandleSaveNutrition childId data ->
            ( { model | saveNutritionRequest = Dict.insert childId data model.saveNutritionRequest }
            , bindDropZone ()
            , []
            )
