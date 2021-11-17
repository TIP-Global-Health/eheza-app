module Pages.TraceContact.Update exposing (update)

import App.Model
import Backend.Model
import Pages.TraceContact.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetTraceContactStep step ->
            ( { model | step = step }
            , Cmd.none
            , []
            )

        SetContactInitiated value ->
            case model.step of
                StepInitiateContact data ->
                    let
                        updatedData =
                            { data | contactInitiated = Just value }
                    in
                    ( { model | step = StepInitiateContact updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SetNoContactReason value ->
            case model.step of
                StepInitiateContact data ->
                    let
                        updatedData =
                            { data | noContactReason = Just value }
                    in
                    ( { model | step = StepInitiateContact updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SaveStepInitiateContact ->
            ( { model | step = StepRecordSymptoms }
            , Cmd.none
            , []
            )
