module Pages.PregnancyOutcome.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Decoder exposing (pregnancyOutcomeFromString)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page(..))
import Pages.PregnancyOutcome.Model exposing (..)


update : NominalDate -> IndividualEncounterParticipantId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    let
        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
        NoOp ->
            noChange

        SavePregnancyOutcome destinationPage ->
            Maybe.map3
                (\dateConcluded outcome isFacilityDelivery ->
                    ( model
                    , Cmd.none
                    , [ Backend.IndividualEncounterParticipant.Model.ClosePrenatalSession dateConcluded outcome isFacilityDelivery
                            |> Backend.Model.MsgIndividualSession id
                            |> App.Model.MsgIndexedDb
                      , App.Model.SetActivePage destinationPage
                      ]
                    )
                )
                model.pregnancyConcludedDate
                model.pregnancyOutcome
                model.isFacilityDelivery
                |> Maybe.withDefault noChange

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
