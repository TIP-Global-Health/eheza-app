module Backend.ChildScoreboardEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.ChildScoreboardEncounter.Model exposing (..)
import Backend.Endpoints exposing (..)
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

        SaveHeight personId valueId value ->
            ( { model | saveHeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardHeightEndpoint HandleSavedHeight
            , []
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardMuacEndpoint HandleSavedMuac
            , []
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardNutritionEndpoint HandleSavedNutrition
            , []
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveWeight personId valueId value ->
            ( { model | saveWeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value childScoreboardWeightEndpoint HandleSavedWeight
            , []
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
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
