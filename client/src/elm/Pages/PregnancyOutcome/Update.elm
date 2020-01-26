module Pages.PregnancyOutcome.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.PrenatalParticipant.Decoder exposing (pregnancyOutcomeFromString)
import Gizra.NominalDate exposing (NominalDate)
import Pages.PregnancyOutcome.Model exposing (..)


update : NominalDate -> PrenatalParticipantId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDeliveryLocation value ->
            ( { model | isFacilityDelivery = Just value }
            , Cmd.none
            , []
            )

        SetPregnancyConcludedDate value ->
            ( { model | pregnancyConcludedDate = Just value }
            , Cmd.none
            , []
            )

        SetPregnancyOutcome value ->
            let
                outcome =
                    pregnancyOutcomeFromString value
            in
            ( { model | pregnancyOutcome = outcome }
            , Cmd.none
            , []
            )

        ToggleDateSelector ->
            ( { model | isDateSelectorOpen = not model.isDateSelectorOpen }
            , Cmd.none
            , []
            )
