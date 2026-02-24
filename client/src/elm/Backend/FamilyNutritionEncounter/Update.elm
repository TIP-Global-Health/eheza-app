module Backend.FamilyNutritionEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.FamilyNutritionEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> FamilyNutritionEncounterId
    -> Maybe FamilyNutritionEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseFamilyNutritionEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | updateFamilyNutritionEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull familyNutritionEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleUpdatedFamilyNutritionEncounter)
                        , []
                        )
                    )

        HandleSavedAhezaChild data ->
            ( { model | saveAhezaChild = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSavedAhezaMother data ->
            ( { model | saveAhezaMother = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSavedMuacChild data ->
            ( { model | saveMuacChild = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleSavedMuacMother data ->
            ( { model | saveMuacMother = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        HandleUpdatedFamilyNutritionEncounter data ->
            ( { model | updateFamilyNutritionEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveAhezaChild personId valueId value ->
            ( { model | saveAhezaChild = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ahezaChildEndpoint HandleSavedAhezaChild
            , []
            )

        SaveAhezaMother personId valueId value ->
            ( { model | saveAhezaMother = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value ahezaMotherEndpoint HandleSavedAhezaMother
            , []
            )

        SaveMuacChild personId valueId value ->
            ( { model | saveMuacChild = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value familyNutritionMuacChildEndpoint HandleSavedMuacChild
            , []
            )

        SaveMuacMother personId valueId value ->
            ( { model | saveMuacMother = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value familyNutritionMuacMotherEndpoint HandleSavedMuacMother
            , []
            )
