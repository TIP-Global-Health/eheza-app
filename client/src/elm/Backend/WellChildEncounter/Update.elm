module Backend.WellChildEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Backend.WellChildEncounter.Encoder exposing (encodeWellChildEncounter)
import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> WellChildEncounterId -> Maybe WellChildEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseWellChildEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeWellChildEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull wellChildEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedWellChildEncounter)
                        )
                    )

        HandleClosedWellChildEncounter data ->
            ( { model | closeWellChildEncounter = data }
            , Cmd.none
            )

        SaveECD personId valueId value ->
            ( { model | saveECD = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildECDEndpoint HandleSavedECD
            )

        HandleSavedECD data ->
            ( { model | saveECD = data }
            , Cmd.none
            )

        SaveHeight personId valueId value ->
            ( { model | saveHeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHeightEndpoint HandleSavedHeight
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMuacEndpoint HandleSavedMuac
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildNutritionEndpoint HandleSavedNutrition
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            )

        SavePhoto personId valueId value ->
            ( { model | savePhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPhotoEndpoint HandleSavedPhoto
            )

        HandleSavedPhoto data ->
            ( { model | savePhoto = data }
            , Cmd.none
            )

        SaveWeight personId valueId value ->
            ( { model | saveWeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildWeightEndpoint HandleSavedWeight
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
            , Cmd.none
            )

        SaveContributingFactors personId valueId value ->
            ( { model | saveContributingFactors = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildContributingFactorsEndpoint HandleSavedContributingFactors
            )

        HandleSavedContributingFactors data ->
            ( { model | saveContributingFactors = data }
            , Cmd.none
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHealthEducationEndpoint HandleSavedHealthEducation
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildFollowUpEndpoint HandleSavedFollowUp
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildSendToHCEndpoint HandleSavedSendToHC
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            )

        SaveHeadCircumference personId valueId value ->
            ( { model | saveHeadCircumference = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHeadCircumferenceEndpoint HandleSavedHeadCircumference
            )

        HandleSavedHeadCircumference data ->
            ( { model | saveHeadCircumference = data }
            , Cmd.none
            )
