module Backend.Session.Update exposing (update)

import App.Ports exposing (bindDropZone)
import AssocList as Dict
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Session.Encoder exposing (encodeSession)
import Backend.Session.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> SessionId -> Maybe Session -> NominalDate -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List MsgIndexedDb )
update nurseId sessionId maybeSession currentDate db msg model =
    case msg of
        CloseSession ->
            unwrap
                ( model, Cmd.none, [] )
                (\session ->
                    ( { model | closeSessionRequest = Loading }
                    , { session | endDate = Just currentDate }
                        |> sw.patchFull sessionEndpoint sessionId
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

        MeasurementOutMsgChild personId subMsg ->
            case subMsg of
                FetchIndividualNutritionData id ->
                    ( model, Cmd.none, Backend.NutritionEncounter.Fetch.fetchForChild id db )

                SaveHeight valueId value ->
                    ( { model | saveHeightRequest = Dict.insert personId Loading model.saveHeightRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value heightEndpoint (HandleSaveHeight personId)
                    , []
                    )

                SaveWeight valueId value ->
                    ( { model | saveWeightRequest = Dict.insert personId Loading model.saveWeightRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value weightEndpoint (HandleSaveWeight personId)
                    , []
                    )

                SaveMuac valueId value ->
                    ( { model | saveMuacRequest = Dict.insert personId Loading model.saveMuacRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value muacEndpoint (HandleSaveMuac personId)
                    , []
                    )

                SaveCounselingSession valueId timing topics ->
                    ( { model | saveCounselingSessionRequest = Dict.insert personId Loading model.saveCounselingSessionRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId ( timing, topics ) counselingSessionEndpoint (HandleSaveCounselingSession personId)
                    , []
                    )

                SaveChildNutritionSigns valueId value ->
                    ( { model | saveNutritionRequest = Dict.insert personId Loading model.saveNutritionRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value nutritionEndpoint (HandleSaveNutrition personId)
                    , []
                    )

                SavePhoto valueId value ->
                    ( { model | savePhotoRequest = Dict.insert personId Loading model.savePhotoRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value photoEndpoint (HandleSavePhoto personId)
                    , []
                    )

                SaveChildFbf valueId value ->
                    ( { model | saveFbfRequest = Dict.insert personId Loading model.saveLactationRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value childFbfEndpoint (HandleSaveFbf personId)
                    , []
                    )

        -- We're handling responses in order to pick up error conditions.
        -- However, we'll let the general "handleRevision" mechanism handle
        -- integrating the data into our model ... we need that anyway, so
        -- there's no point having two separate code paths to do it.
        MeasurementOutMsgMother personId subMsg ->
            case subMsg of
                SaveAttendance valueId value ->
                    ( { model | saveAttendanceRequest = Dict.insert personId Loading model.saveAttendanceRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value attendanceEndpoint (HandleSaveAttendance personId)
                    , []
                    )

                SaveFamilyPlanningSigns valueId value ->
                    ( { model | saveFamilyPlanningRequest = Dict.insert personId Loading model.saveFamilyPlanningRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value familyPlanningEndpoint (HandleSaveFamilyPlanning personId)
                    , []
                    )

                SaveLactation valueId value ->
                    ( { model | saveLactationRequest = Dict.insert personId Loading model.saveLactationRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value lactationEndpoint (HandleSaveLactation personId)
                    , []
                    )

                SaveMotherFbf valueId value ->
                    ( { model | saveFbfRequest = Dict.insert personId Loading model.saveLactationRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value motherFbfEndpoint (HandleSaveFbf personId)
                    , []
                    )

                SaveCompletedForm valueId formId language ->
                    let
                        value =
                            { formId = formId
                            , language = language
                            }
                    in
                    ( { model | saveParticipantConsentRequest = Dict.insert personId Loading model.saveParticipantConsentRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value participantConsentEndpoint (HandleSaveParticipantConsent personId)
                    , []
                    )

        HandleSaveAttendance personId data ->
            ( { model | saveAttendanceRequest = Dict.insert personId data model.saveAttendanceRequest }
            , Cmd.none
            , []
            )

        HandleSaveCounselingSession personId data ->
            ( { model | saveCounselingSessionRequest = Dict.insert personId data model.saveCounselingSessionRequest }
            , Cmd.none
            , []
            )

        HandleSaveParticipantConsent personId data ->
            ( { model | saveParticipantConsentRequest = Dict.insert personId data model.saveParticipantConsentRequest }
            , Cmd.none
            , []
            )

        HandleSaveFamilyPlanning personId data ->
            ( { model | saveFamilyPlanningRequest = Dict.insert personId data model.saveFamilyPlanningRequest }
            , Cmd.none
            , []
            )

        HandleSaveLactation personId data ->
            ( { model | saveLactationRequest = Dict.insert personId data model.saveLactationRequest }
            , Cmd.none
            , []
            )

        HandleSaveFbf personId data ->
            ( { model | saveFbfRequest = Dict.insert personId data model.saveFbfRequest }
            , Cmd.none
            , []
            )

        HandleSaveHeight personId data ->
            ( { model | saveHeightRequest = Dict.insert personId data model.saveHeightRequest }
            , bindDropZone ()
            , []
            )

        HandleSaveWeight personId data ->
            ( { model | saveWeightRequest = Dict.insert personId data model.saveWeightRequest }
            , bindDropZone ()
            , []
            )

        HandleSaveMuac personId data ->
            ( { model | saveMuacRequest = Dict.insert personId data model.saveMuacRequest }
            , bindDropZone ()
            , []
            )

        HandleSavePhoto personId data ->
            ( { model | savePhotoRequest = Dict.insert personId data model.savePhotoRequest }
            , bindDropZone ()
            , []
            )

        HandleSaveNutrition personId data ->
            ( { model | saveNutritionRequest = Dict.insert personId data model.saveNutritionRequest }
            , bindDropZone ()
            , []
            )
