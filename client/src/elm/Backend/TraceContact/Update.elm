module Backend.TraceContact.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (AcuteIllnessTraceContact)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TraceContact.Model exposing (..)
import Backend.Utils exposing (editMeasurementCmd)
import RemoteData exposing (RemoteData(..))


update : AcuteIllnessTraceContactId -> Maybe AcuteIllnessTraceContact -> Msg -> Model -> ( Model, Cmd Msg )
update id traceContact msg model =
    case msg of
        EditTraceContact value ->
            ( { model | saveTraceContact = Loading }
            , Maybe.map
                (editMeasurementCmd id
                    value
                    acuteIllnessTraceContactEndpoint
                    HandleSavedTraceContact
                )
                traceContact
                |> Maybe.withDefault Cmd.none
            )

        HandleSavedTraceContact data ->
            ( { model | saveTraceContact = data }
            , Cmd.none
            )
