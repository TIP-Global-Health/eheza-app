module Backend.TraceContact.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (acuteIllnessTraceContactEndpoint)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (AcuteIllnessTraceContact)
import Backend.TraceContact.Model exposing (Model, Msg(..))
import Backend.Utils exposing (editMeasurementCmd)
import RemoteData exposing (RemoteData(..))


update : AcuteIllnessTraceContactId -> Maybe AcuteIllnessTraceContact -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
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
            , []
            )

        HandleSavedTraceContact data ->
            ( { model | saveTraceContact = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
