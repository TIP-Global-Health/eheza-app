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
                            |> sw.patchAny nutritionEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedAcuteIllnessEncounter)
                        )
                    )

        HandleClosedAcuteIllnessEncounter data ->
            ( { model | closeAcuteIllnessEncounter = data }
            , Cmd.none
            )
