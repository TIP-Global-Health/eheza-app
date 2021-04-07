module Pages.HomeVisitActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model
import Backend.IndividualEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.HomeVisitActivity.Model exposing (..)
import Pages.HomeVisitActivity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> HomeVisitEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetFeedingBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.feedingForm
            in
            ( { model | feedingForm = updatedForm }
            , Cmd.none
            , []
            )

        SetNutritionSupplementType value ->
            let
                form =
                    model.feedingForm

                updatedForm =
                    { form | supplementType = Just value }
            in
            ( { model | feedingForm = updatedForm }
            , Cmd.none
            , []
            )

        SetSachetsPerDay value ->
            let
                form =
                    model.feedingForm

                updatedForm =
                    { form | sachetsPerDay = String.toFloat value }
            in
            ( { model | feedingForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveFeeding personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.feedingForm
                        |> toNutritionFeedingValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.HomeVisitEncounter.Model.SaveFeeding personId measurementId value
                                    |> Backend.Model.MsgHomeVisitEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| HomeVisitEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
