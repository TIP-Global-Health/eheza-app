module Pages.ChildScoreboard.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.ChildScoreboardEncounter.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (WeightInGrm(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (toNCDAValueWithDefault)
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))


update : NominalDate -> ChildScoreboardEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetNCDABoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.ncdaData.form

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetBirthWeight string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | birthWeight = String.toFloat string |> Maybe.map WeightInGrm
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNumberANCVisits string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | numberOfANCVisits = String.toInt string })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNutritionSupplementType value ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | foodSupplementType = Just value, takingFoodSupplements = Nothing })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNCDAFormStep step ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | step = Just step })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNCDAHelperState state ->
            let
                updatedData =
                    model.ncdaData
                        |> (\data -> { data | helperState = state })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SaveNCDA personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.ncdaData.form
                        |> toNCDAValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.ChildScoreboardEncounter.Model.SaveNCDA personId measurementId value
                                    |> Backend.Model.MsgChildScoreboardEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| ChildScoreboardEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
