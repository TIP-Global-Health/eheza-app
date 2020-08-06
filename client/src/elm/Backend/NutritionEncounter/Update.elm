module Backend.NutritionEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
import Backend.NutritionEncounter.Encoder exposing (encodeNutritionEncounter)
import Backend.NutritionEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> NutritionEncounterId -> Maybe NutritionEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    let
        sw =
            applyBackendUrl "/sw"

        saveMeasurementCmd date encounter person nurse savedValueId savedValue valueEncoder endpoint handleSavedMsg =
            let
                measurement =
                    { participantId = person
                    , dateMeasured = date
                    , encounterId = Just encounter
                    , nurse = nurse
                    , healthCenter = healthCenterId
                    , value = savedValue
                    }

                requestData =
                    case savedValueId of
                        Nothing ->
                            measurement
                                |> sw.post endpoint
                                |> withoutDecoder

                        Just id ->
                            measurement
                                |> valueEncoder
                                |> object
                                |> sw.patchAny endpoint id
                                |> withoutDecoder
            in
            toCmd (RemoteData.fromResult >> handleSavedMsg) requestData
    in
    case msg of
        CloseNutritionEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeNutritionEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> encodeNutritionEncounter
                            |> object
                            |> sw.patchAny nutritionEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedNutritionEncounter)
                        )
                    )

        HandleClosedNutritionEncounter data ->
            ( { model | closeNutritionEncounter = data }
            , Cmd.none
            )

        SaveHeight personId valueId value ->
            ( { model | saveHeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId valueId value encodeNutritionHeight nutritionHeightEndpoint HandleSavedHeight
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId valueId value encodeNutritionMuac nutritionMuacEndpoint HandleSavedMuac
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId valueId value encodeNutritionNutrition nutritionNutritionEndpoint HandleSavedNutrition
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            )

        SavePhoto personId valueId value ->
            ( { model | savePhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId valueId value encodeNutritionPhoto nutritionPhotoEndpoint HandleSavedPhoto
            )

        HandleSavedPhoto data ->
            ( { model | savePhoto = data }
            , Cmd.none
            )

        SaveWeight personId valueId value ->
            ( { model | saveWeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId valueId value encodeNutritionWeight nutritionWeightEndpoint HandleSavedWeight
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
            , Cmd.none
            )
