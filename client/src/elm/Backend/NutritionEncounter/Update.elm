module Backend.NutritionEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.NutritionEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> NutritionEncounterId
    -> Maybe NutritionEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseNutritionEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeNutritionEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull nutritionEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedNutritionEncounter)
                        , []
                        )
                    )

        HandleClosedNutritionEncounter data ->
            ( { model | closeNutritionEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHeight personId valueId value ->
            ( { model | saveHeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionHeightEndpoint HandleSavedHeight
            , []
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionMuacEndpoint HandleSavedMuac
            , []
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionNutritionEndpoint HandleSavedNutrition
            , []
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePhoto personId valueId value ->
            ( { model | savePhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionPhotoEndpoint HandleSavedPhoto
            , []
            )

        HandleSavedPhoto data ->
            ( { model | savePhoto = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveWeight personId valueId value ->
            ( { model | saveWeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionWeightEndpoint HandleSavedWeight
            , []
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNCDA personId valueId value ->
            ( { model | saveNCDA = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionNCDAEndpoint HandleSavedNCDA
            , []
            )

        HandleSavedNCDA data ->
            ( { model | saveNCDA = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionSendToHCEndpoint HandleSavedSendToHC
            , []
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionHealthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveContributingFactors personId valueId value ->
            ( { model | saveContributingFactors = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionContributingFactorsEndpoint HandleSavedContributingFactors
            , []
            )

        HandleSavedContributingFactors data ->
            ( { model | saveContributingFactors = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value nutritionFollowUpEndpoint HandleSavedFollowUp
            , []
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
