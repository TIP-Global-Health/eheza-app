module Backend.Session.Update exposing (update)

import App.Model
import App.Ports exposing (bindDropZone)
import App.Utils exposing (triggerRollbarOnFailure)
import AssocList as Dict
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Fetch
import Backend.Session.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : NominalDate -> Maybe NurseId -> SessionId -> Maybe Session -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId sessionId maybeSession db msg model =
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
            , triggerRollbarOnFailure data
            )

        MeasurementOutMsgChild personId subMsg ->
            case subMsg of
                NoOp ->
                    ( model, Cmd.none, [] )

                FetchIndividualNutritionData id ->
                    let
                        appMsgs =
                            Backend.NutritionEncounter.Fetch.fetch id db
                                |> List.map App.Model.MsgIndexedDb
                    in
                    ( model, Cmd.none, appMsgs )

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

                SaveNutrition valueId value ->
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
                    ( { model | saveFbfRequest = Dict.insert personId Loading model.saveFbfRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value childFbfEndpoint (HandleSaveFbf personId)
                    , []
                    )

                SaveContributingFactors valueId value ->
                    ( { model | saveContributingFactorsRequest = Dict.insert personId Loading model.saveContributingFactorsRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value contributingFactorsEndpoint (HandleSaveContributingFactors personId)
                    , []
                    )

                SaveFollowUp valueId value ->
                    ( { model | saveFollowUpRequest = Dict.insert personId Loading model.saveFollowUpRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value followUpEndpoint (HandleSaveFollowUp personId)
                    , []
                    )

                SaveHealthEducation valueId value ->
                    ( { model | saveHealthEducationRequest = Dict.insert personId Loading model.saveHealthEducationRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value groupHealthEducationEndpoint (HandleSaveHealthEducation personId)
                    , []
                    )

                SaveSendToHC valueId value ->
                    ( { model | saveSendToHCRequest = Dict.insert personId Loading model.saveSendToHCRequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value groupSendToHCEndpoint (HandleSaveSendToHC personId)
                    , []
                    )

                SaveNCDA valueId value ->
                    ( { model | saveNCDARequest = Dict.insert personId Loading model.saveNCDARequest }
                    , saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value groupNCDAEndpoint (HandleSaveNCDA personId)
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
            , triggerRollbarOnFailure data
            )

        HandleSaveCounselingSession personId data ->
            ( { model | saveCounselingSessionRequest = Dict.insert personId data model.saveCounselingSessionRequest }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSaveParticipantConsent personId data ->
            ( { model | saveParticipantConsentRequest = Dict.insert personId data model.saveParticipantConsentRequest }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSaveFamilyPlanning personId data ->
            ( { model | saveFamilyPlanningRequest = Dict.insert personId data model.saveFamilyPlanningRequest }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSaveLactation personId data ->
            ( { model | saveLactationRequest = Dict.insert personId data model.saveLactationRequest }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSaveFbf personId data ->
            ( { model | saveFbfRequest = Dict.insert personId data model.saveFbfRequest }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSaveHeight personId data ->
            ( { model | saveHeightRequest = Dict.insert personId data model.saveHeightRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveWeight personId data ->
            ( { model | saveWeightRequest = Dict.insert personId data model.saveWeightRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveMuac personId data ->
            ( { model | saveMuacRequest = Dict.insert personId data model.saveMuacRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSavePhoto personId data ->
            ( { model | savePhotoRequest = Dict.insert personId data model.savePhotoRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveNutrition personId data ->
            ( { model | saveNutritionRequest = Dict.insert personId data model.saveNutritionRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveContributingFactors personId data ->
            ( { model | saveContributingFactorsRequest = Dict.insert personId data model.saveContributingFactorsRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveFollowUp personId data ->
            ( { model | saveFollowUpRequest = Dict.insert personId data model.saveFollowUpRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveHealthEducation personId data ->
            ( { model | saveHealthEducationRequest = Dict.insert personId data model.saveHealthEducationRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveSendToHC personId data ->
            ( { model | saveSendToHCRequest = Dict.insert personId data model.saveSendToHCRequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )

        HandleSaveNCDA personId data ->
            ( { model | saveNCDARequest = Dict.insert personId data model.saveNCDARequest }
            , bindDropZone ()
            , triggerRollbarOnFailure data
            )
