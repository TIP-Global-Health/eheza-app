module Pages.Prenatal.Outcome.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation(..))
import Backend.IndividualEncounterParticipant.Utils exposing (pregnancyOutcomeFromString)
import Backend.Model
import Gizra.NominalDate exposing (NominalDate)
import Pages.Prenatal.Outcome.Model exposing (..)


update : NominalDate -> IndividualEncounterParticipantId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        SavePregnancyOutcome destinationPage ->
            Maybe.map3
                (\dateConcluded outcome deliveryLocation ->
                    ( model
                    , Cmd.none
                    , [ Backend.IndividualEncounterParticipant.Model.ClosePrenatalSession dateConcluded outcome deliveryLocation
                            |> Backend.Model.MsgIndividualEncounterParticipant id
                            |> App.Model.MsgIndexedDb
                      , App.Model.SetActivePage destinationPage
                      ]
                    )
                )
                model.pregnancyConcludedDate
                model.pregnancyOutcome
                model.deliveryLocation
                |> Maybe.withDefault ( model, Cmd.none, [] )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDeliveryLocation isFacilityDelivery ->
            let
                location =
                    if isFacilityDelivery then
                        FacilityDelivery

                    else
                        HomeDelivery
            in
            ( { model | deliveryLocation = Just location }
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

        SetDateSelectorState state ->
            ( { model | dateSelectorPopupState = state }
            , Cmd.none
            , []
            )
