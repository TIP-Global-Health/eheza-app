module Backend.AcuteIllnessEncounter.Update exposing (update)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, encodeEntityUuid, toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> AcuteIllnessEncounterId -> Maybe AcuteIllnessEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        CloseAcuteIllnessEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | closeAcuteIllnessEncounter = Loading }
                        , object
                            [ ( "scheduled_date"
                              , object
                                    [ ( "value", encodeYYYYMMDD encounter.startDate )
                                    , ( "value2", encodeYYYYMMDD currentDate )
                                    ]
                              )
                            ]
                            |> sw.patchAny acuteIllnessEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedAcuteIllnessEncounter)
                        )
                    )

        HandleClosedAcuteIllnessEncounter data ->
            ( { model | closeAcuteIllnessEncounter = data }
            , Cmd.none
            )

        SaveSymptomsGeneral personId valueId value ->
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
                                |> sw.post symptomsGeneralEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGeneral)

                        Just id ->
                            encodeSymptomsGeneralValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny symptomsGeneralEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGeneral)
            in
            ( { model | saveSymptomsGeneral = Loading }
            , cmd
            )

        HandleSavedSymptomsGeneral data ->
            ( { model | saveSymptomsGeneral = data }
            , Cmd.none
            )

        SaveSymptomsRespiratory personId valueId value ->
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
                                |> sw.post symptomsRespiratoryEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsRespiratory)

                        Just id ->
                            encodeSymptomsRespiratoryValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny symptomsRespiratoryEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsRespiratory)
            in
            ( { model | saveSymptomsRespiratory = Loading }
            , cmd
            )

        HandleSavedSymptomsRespiratory data ->
            ( { model | saveSymptomsRespiratory = data }
            , Cmd.none
            )

        SaveSymptomsGI personId valueId value ->
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
                                |> sw.post symptomsGIEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGI)

                        Just id ->
                            encodeSymptomsGIValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny symptomsGIEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedSymptomsGI)
            in
            ( { model | saveSymptomsGI = Loading }
            , cmd
            )

        HandleSavedSymptomsGI data ->
            ( { model | saveSymptomsGI = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
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
                                |> sw.post acuteIllnessVitalsEndpoint
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedVitals)

                        Just id ->
                            encodeAcuteIllnessVitalsValue value
                                |> List.append
                                    [ ( "nurse", Json.Encode.Extra.maybe encodeEntityUuid nurseId )
                                    , ( "health_center", Json.Encode.Extra.maybe encodeEntityUuid healthCenterId )
                                    ]
                                |> object
                                |> sw.patchAny acuteIllnessVitalsEndpoint id
                                |> withoutDecoder
                                |> toCmd (RemoteData.fromResult >> HandleSavedVitals)
            in
            ( { model | saveVitals = Loading }
            , cmd
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )
