module Backend.PrenatalParticipant.Update exposing (update)

import Backend.Endpoints exposing (prenatalParticipantEndpoint)
import Backend.Entities exposing (PrenatalParticipantId)
import Backend.PrenatalParticipant.Encoder exposing (..)
import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD, fromLocalDateTime)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, toCmd, withoutDecoder)


update : PrenatalParticipantId -> Maybe PrenatalParticipant -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update participantId maybeParticipant currentDate msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        ClosePrenatalSession pregnancyConcludedDate pregnancyOutcome isFacilityDelivery ->
            maybeParticipant
                |> unwrap ( model, Cmd.none )
                    (\participant ->
                        ( { model | closePrenatalSession = Loading }
                        , object
                            [ ( "expected"
                              , object
                                    [ ( "value", encodeYYYYMMDD participant.startDate )
                                    , ( "value2", encodeYYYYMMDD currentDate )
                                    ]
                              )
                            , ( "date_concluded", encodeYYYYMMDD (fromLocalDateTime pregnancyConcludedDate) )
                            , ( "outcome", encodePregnancyOutcome pregnancyOutcome )
                            , ( "outcome_location", encodeDeliveryLocation isFacilityDelivery )
                            ]
                            |> sw.patchAny prenatalParticipantEndpoint participantId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedPrenatalSession)
                        )
                    )

        HandleClosedPrenatalSession data ->
            ( { model | closePrenatalSession = data }
            , Cmd.none
            )
