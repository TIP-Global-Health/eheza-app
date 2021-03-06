module Backend.IndividualEncounterParticipant.Update exposing (update)

import Backend.Endpoints exposing (individualEncounterParticipantEndpoint)
import Backend.Entities exposing (IndividualEncounterParticipantId)
import Backend.IndividualEncounterParticipant.Encoder exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, toCmd, withoutDecoder)


update : IndividualEncounterParticipantId -> Maybe IndividualEncounterParticipant -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update participantId maybeParticipant currentDate msg model =
    case msg of
        ClosePrenatalSession concludedDate outcome deliveryLocation ->
            maybeParticipant
                |> unwrap ( model, Cmd.none )
                    (\participant ->
                        ( { model | closePrenatalSession = Loading }
                        , { participant | endDate = Just currentDate, dateConcluded = Just concludedDate, outcome = Just (Pregnancy outcome), deliveryLocation = Just deliveryLocation }
                            |> sw.patchFull individualEncounterParticipantEndpoint participantId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedPrenatalSession)
                        )
                    )

        HandleClosedPrenatalSession data ->
            ( { model | closePrenatalSession = data }
            , Cmd.none
            )

        CloseAcuteIllnessSession outcome ->
            maybeParticipant
                |> unwrap ( model, Cmd.none )
                    (\participant ->
                        ( { model | closeAcuteIllnessSession = Loading }
                        , { participant | endDate = Just currentDate, outcome = Just (AcuteIllness outcome) }
                            |> sw.patchFull individualEncounterParticipantEndpoint participantId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedPrenatalSession)
                        )
                    )

        HandleClosedAcuteIllnessSession data ->
            ( { model | closeAcuteIllnessSession = data }
            , Cmd.none
            )

        SetEddDate eddDate ->
            maybeParticipant
                |> unwrap ( model, Cmd.none )
                    (\participant ->
                        ( { model | setEddDate = Loading }
                        , { participant | eddDate = Just eddDate }
                            |> sw.patchFull individualEncounterParticipantEndpoint participantId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleSetEddDate)
                        )
                    )

        HandleSetEddDate data ->
            ( { model | setEddDate = data }
            , Cmd.none
            )

        SetNewborn personId ->
            maybeParticipant
                |> unwrap ( model, Cmd.none )
                    (\participant ->
                        ( { model | setNewborn = Loading }
                        , { participant | newborn = Just personId }
                            |> sw.patchFull individualEncounterParticipantEndpoint participantId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleSetNewborn)
                        )
                    )

        HandleSetNewborn data ->
            ( { model | setNewborn = data }
            , Cmd.none
            )
