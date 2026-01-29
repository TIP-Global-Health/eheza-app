module Backend.HomeVisitEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (homeVisitEncounterEndpoint, nutritionCaringEndpoint, nutritionFeedingEndpoint, nutritionFoodSecurityEndpoint, nutritionHygieneEndpoint)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter, Model, Msg(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> HomeVisitEncounterId
    -> Maybe HomeVisitEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseHomeVisitEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeHomeVisitEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull homeVisitEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedHomeVisitEncounter)
                        , []
                        )
                    )

        HandleClosedHomeVisitEncounter data ->
            ( { model | closeHomeVisitEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFeeding personId valueId value ->
            ( { model | saveFeeding = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionFeedingEndpoint HandleSavedFeeding
            , []
            )

        HandleSavedFeeding data ->
            ( { model | saveFeeding = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHygiene personId valueId value ->
            ( { model | saveHygiene = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionHygieneEndpoint HandleSavedHygiene
            , []
            )

        HandleSavedHygiene data ->
            ( { model | saveHygiene = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFoodSecurity personId valueId value ->
            ( { model | saveFoodSecurity = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionFoodSecurityEndpoint HandleSavedFoodSecurity
            , []
            )

        HandleSavedFoodSecurity data ->
            ( { model | saveFoodSecurity = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCaring personId valueId value ->
            ( { model | saveCaring = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionCaringEndpoint HandleSavedCaring
            , []
            )

        HandleSavedCaring data ->
            ( { model | saveCaring = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
