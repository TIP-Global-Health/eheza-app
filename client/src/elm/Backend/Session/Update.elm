module Backend.Session.Update exposing (update)

import App.Model
import App.Ports exposing (bindDropZone)
import App.Utils exposing (triggerRollbarOnFailure)
import AssocList as Dict exposing (Dict)
import Backend.Endpoints exposing (attendanceEndpoint, childFbfEndpoint, contributingFactorsEndpoint, counselingSessionEndpoint, familyPlanningEndpoint, followUpEndpoint, groupHealthEducationEndpoint, groupNCDAEndpoint, groupSendToHCEndpoint, heightEndpoint, lactationEndpoint, motherFbfEndpoint, muacEndpoint, nutritionEndpoint, participantConsentEndpoint, photoEndpoint, sessionEndpoint, weightEndpoint)
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (Model, Msg(..), Session)
import Backend.Session.Utils exposing (isSaveAllowed, saveRequestOf)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : NominalDate -> Maybe NurseId -> SessionId -> Maybe Session -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId sessionId maybeSession msg model =
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

                SaveHeight valueId value ->
                    startSave personId valueId .saveHeightRequest (\requests -> { model | saveHeightRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value heightEndpoint (HandleSaveHeight personId)) model

                SaveWeight valueId value ->
                    startSave personId valueId .saveWeightRequest (\requests -> { model | saveWeightRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value weightEndpoint (HandleSaveWeight personId)) model

                SaveMuac valueId value ->
                    startSave personId valueId .saveMuacRequest (\requests -> { model | saveMuacRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value muacEndpoint (HandleSaveMuac personId)) model

                SaveCounselingSession valueId timing topics ->
                    startSave personId valueId .saveCounselingSessionRequest (\requests -> { model | saveCounselingSessionRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId ( timing, topics ) counselingSessionEndpoint (HandleSaveCounselingSession personId)) model

                SaveNutrition valueId value ->
                    startSave personId valueId .saveNutritionRequest (\requests -> { model | saveNutritionRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value nutritionEndpoint (HandleSaveNutrition personId)) model

                SavePhoto valueId value ->
                    startSave personId valueId .savePhotoRequest (\requests -> { model | savePhotoRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value photoEndpoint (HandleSavePhoto personId)) model

                SaveChildFbf valueId value ->
                    startSave personId valueId .saveFbfRequest (\requests -> { model | saveFbfRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value childFbfEndpoint (HandleSaveFbf personId)) model

                SaveContributingFactors valueId value ->
                    startSave personId valueId .saveContributingFactorsRequest (\requests -> { model | saveContributingFactorsRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value contributingFactorsEndpoint (HandleSaveContributingFactors personId)) model

                SaveFollowUp valueId value ->
                    startSave personId valueId .saveFollowUpRequest (\requests -> { model | saveFollowUpRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value followUpEndpoint (HandleSaveFollowUp personId)) model

                SaveHealthEducation valueId value ->
                    startSave personId valueId .saveHealthEducationRequest (\requests -> { model | saveHealthEducationRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value groupHealthEducationEndpoint (HandleSaveHealthEducation personId)) model

                SaveSendToHC valueId value ->
                    startSave personId valueId .saveSendToHCRequest (\requests -> { model | saveSendToHCRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value groupSendToHCEndpoint (HandleSaveSendToHC personId)) model

                SaveNCDA valueId value ->
                    startSave personId valueId .saveNCDARequest (\requests -> { model | saveNCDARequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value groupNCDAEndpoint (HandleSaveNCDA personId)) model

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
                    startSave personId valueId .saveFamilyPlanningRequest (\requests -> { model | saveFamilyPlanningRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value familyPlanningEndpoint (HandleSaveFamilyPlanning personId)) model

                SaveLactation valueId value ->
                    startSave personId valueId .saveLactationRequest (\requests -> { model | saveLactationRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value lactationEndpoint (HandleSaveLactation personId)) model

                SaveMotherFbf valueId value ->
                    startSave personId valueId .saveFbfRequest (\requests -> { model | saveFbfRequest = requests }) (saveMeasurementCmd currentDate sessionId personId nurseId Nothing valueId value motherFbfEndpoint (HandleSaveFbf personId)) model

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


{-| Sends a save and marks it in progress, when `isSaveAllowed` lets it go ahead.
-}
startSave :
    PersonId
    -> Maybe valueId
    -> (Model -> Dict PersonId (WebData ()))
    -> (Dict PersonId (WebData ()) -> Model)
    -> Cmd Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
startSave personId valueId getRequests setRequests cmd model =
    let
        requests =
            getRequests model
    in
    if isSaveAllowed valueId (saveRequestOf personId requests) then
        ( setRequests (Dict.insert personId Loading requests), cmd, [] )

    else
        ( model, Cmd.none, [] )
