module Backend.NutritionEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
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
    in
    case msg of
        CloseNutritionEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeNutritionEncounter = Loading }
                        , object
                            [ ( "scheduled_date"
                              , object
                                    [ ( "value", encodeYYYYMMDD encounter.startDate )
                                    , ( "value2", encodeYYYYMMDD currentDate )
                                    ]
                              )
                            ]
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
            let
                cmd =
                    case valueId of
                        Nothing ->
                            { participantId = personId
                            , dateMeasured = currentDate
                            , encounterId = Just encounterId
                            , nurse = nurseId
                            , healthCenter = healthCenterId
                            , value = value
                            }
                                |> sw.post nutritionHeightEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedHeight)

                        Just id ->
                            encodeNutritionHeightValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny nutritionHeightEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedHeight)
            in
            ( { model | saveHeight = Loading }
            , cmd
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            )

        SaveMuac personId valueId value ->
            let
                cmd =
                    case valueId of
                        Nothing ->
                            { participantId = personId
                            , dateMeasured = currentDate
                            , encounterId = Just encounterId
                            , nurse = nurseId
                            , healthCenter = healthCenterId
                            , value = value
                            }
                                |> sw.post nutritionMuacEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMuac)

                        Just id ->
                            encodeNutritionMuacValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny nutritionMuacEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedMuac)
            in
            ( { model | saveMuac = Loading }
            , cmd
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            )

        SaveNutrition personId valueId value ->
            let
                cmd =
                    case valueId of
                        Nothing ->
                            { participantId = personId
                            , dateMeasured = currentDate
                            , encounterId = Just encounterId
                            , nurse = nurseId
                            , healthCenter = healthCenterId
                            , value = value
                            }
                                |> sw.post nutritionNutritionEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedNutrition)

                        Just id ->
                            encodeNutritionValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny nutritionNutritionEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedNutrition)
            in
            ( { model | saveNutrition = Loading }
            , cmd
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            )

        SavePhoto personId valueId value ->
            let
                cmd =
                    case valueId of
                        Nothing ->
                            { participantId = personId
                            , dateMeasured = currentDate
                            , encounterId = Just encounterId
                            , nurse = nurseId
                            , healthCenter = healthCenterId
                            , value = value
                            }
                                |> sw.post nutritionPhotoEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedPhoto)

                        Just id ->
                            encodePhotoUrl value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny nutritionPhotoEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedPhoto)
            in
            ( { model | savePhoto = Loading }
            , cmd
            )

        HandleSavedPhoto data ->
            ( { model | savePhoto = data }
            , Cmd.none
            )

        SaveWeight personId valueId value ->
            let
                cmd =
                    case valueId of
                        Nothing ->
                            { participantId = personId
                            , dateMeasured = currentDate
                            , encounterId = Just encounterId
                            , nurse = nurseId
                            , healthCenter = healthCenterId
                            , value = value
                            }
                                |> sw.post nutritionWeightEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedWeight)

                        Just id ->
                            encodeNutritionWeightValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny nutritionWeightEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedWeight)
            in
            ( { model | saveWeight = Loading }
            , cmd
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
            , Cmd.none
            )
