module Pages.ChildScoreboard.Encounter.Update exposing (update)

import App.Model
import Backend.ChildScoreboardEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (WeightInGrm(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils exposing (toNCDAValueNEWWithDefault)
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter assembled ->
            let
                measurementId =
                    Maybe.map Tuple.first assembled.measurements.ncda

                measurement =
                    getMeasurementValueFunc assembled.measurements.ncda

                saveNCDAMsg =
                    model.ncdaData.form
                        |> toNCDAValueNEWWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.ChildScoreboardEncounter.Model.SaveNCDA assembled.participant.person measurementId value
                                    |> Backend.Model.MsgChildScoreboardEncounter assembled.id
                                    |> App.Model.MsgIndexedDb
                                ]
                            )
            in
            ( model
            , Cmd.none
            , [ Backend.ChildScoreboardEncounter.Model.CloseChildScoreboardEncounter
                    |> Backend.Model.MsgChildScoreboardEncounter assembled.id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
                ++ saveNCDAMsg
            )

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

        ShowAIEncounterPopup ->
            ( { model | showAIEncounterPopup = True }
            , Cmd.none
            , []
            )

        TriggerAcuteIllnessEncounter assembled ->
            ( { model | showAIEncounterPopup = False }
            , Cmd.none
            , [ App.Model.SetActivePage <| UserPage (AcuteIllnessParticipantPage InitiatorParticipantsPage assembled.participant.person) ]
            )
                |> sequenceExtra update [ CloseEncounter assembled ]
