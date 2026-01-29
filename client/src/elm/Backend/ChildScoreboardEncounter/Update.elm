module Backend.ChildScoreboardEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.ChildScoreboardEncounter.Model exposing (ChildScoreboardEncounter, Model, Msg(..))
import Backend.Endpoints exposing (childScoreboardBCGImmunisationEndpoint, childScoreboardDTPImmunisationEndpoint, childScoreboardDTPStandaloneImmunisationEndpoint, childScoreboardEncounterEndpoint, childScoreboardIPVImmunisationEndpoint, childScoreboardMRImmunisationEndpoint, childScoreboardNCDAEndpoint, childScoreboardOPVImmunisationEndpoint, childScoreboardPCV13ImmunisationEndpoint, childScoreboardRotarixImmunisationEndpoint)
import Backend.Entities exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> ChildScoreboardEncounterId
    -> Maybe ChildScoreboardEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseChildScoreboardEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeChildScoreboardEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull childScoreboardEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedChildScoreboardEncounter)
                        , []
                        )
                    )

        HandleClosedChildScoreboardEncounter data ->
            ( { model | closeChildScoreboardEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNCDA personId valueId value ->
            ( { model | saveNCDA = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardNCDAEndpoint HandleSavedNCDA
            , []
            )

        HandleSavedNCDA data ->
            ( { model | saveNCDA = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveBCGImmunisation personId valueId value ->
            ( { model | saveBCGImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardBCGImmunisationEndpoint HandleSavedBCGImmunisation
            , []
            )

        HandleSavedBCGImmunisation data ->
            ( { model | saveBCGImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDTPImmunisation personId valueId value ->
            ( { model | saveDTPImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardDTPImmunisationEndpoint HandleSavedDTPImmunisation
            , []
            )

        HandleSavedDTPImmunisation data ->
            ( { model | saveDTPImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDTPStandaloneImmunisation personId valueId value ->
            ( { model | saveDTPStandaloneImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardDTPStandaloneImmunisationEndpoint HandleSavedDTPStandaloneImmunisation
            , []
            )

        HandleSavedDTPStandaloneImmunisation data ->
            ( { model | saveDTPStandaloneImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveIPVImmunisation personId valueId value ->
            ( { model | saveIPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardIPVImmunisationEndpoint HandleSavedIPVImmunisation
            , []
            )

        HandleSavedIPVImmunisation data ->
            ( { model | saveIPVImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMRImmunisation personId valueId value ->
            ( { model | saveMRImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardMRImmunisationEndpoint HandleSavedMRImmunisation
            , []
            )

        HandleSavedMRImmunisation data ->
            ( { model | saveMRImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveOPVImmunisation personId valueId value ->
            ( { model | saveOPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardOPVImmunisationEndpoint HandleSavedOPVImmunisation
            , []
            )

        HandleSavedOPVImmunisation data ->
            ( { model | saveOPVImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePCV13Immunisation personId valueId value ->
            ( { model | savePCV13Immunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardPCV13ImmunisationEndpoint HandleSavedPCV13Immunisation
            , []
            )

        HandleSavedPCV13Immunisation data ->
            ( { model | savePCV13Immunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveRotarixImmunisation personId valueId value ->
            ( { model | saveRotarixImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardRotarixImmunisationEndpoint HandleSavedRotarixImmunisation
            , []
            )

        HandleSavedRotarixImmunisation data ->
            ( { model | saveRotarixImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
