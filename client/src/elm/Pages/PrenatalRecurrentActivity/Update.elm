module Pages.PrenatalRecurrentActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalRecurrentActivity.Model exposing (..)
import Pages.PrenatalRecurrentActivity.Utils exposing (..)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        generateNextTaskMsgs nextTask =
            Maybe.map (\task -> [ SetActiveTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id ]
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState value ->
            ( { model | showAlertsDialog = value }, Cmd.none, [] )

        SetActiveTask task ->
            ( { model | activeTask = Just task }
            , Cmd.none
            , []
            )

        SetSyphilisTestResult value ->
            let
                form =
                    model.syphilisTestForm

                updatedForm =
                    { form | testResult = prenatalTestResultFromString value }
            in
            ( { model | syphilisTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveSyphilisResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextTaskMsgs nextTask

                appMsgs =
                    toPrenatalTestResultsValueWithDefault measurement model.syphilisTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSyphilisTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetHepatitisBTestResult value ->
            let
                form =
                    model.hepatitisBTestForm

                updatedForm =
                    { form | testResult = prenatalTestResultFromString value }
            in
            ( { model | hepatitisBTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveHepatitisBResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextTaskMsgs nextTask

                appMsgs =
                    toPrenatalTestResultsValueWithDefault measurement model.hepatitisBTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHepatitisBTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetBloodGroup value ->
            let
                form =
                    model.bloodGpRsTestForm

                updatedForm =
                    { form | bloodGroup = bloodGroupFromString value }
            in
            ( { model | bloodGpRsTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetRhesus value ->
            let
                form =
                    model.bloodGpRsTestForm

                updatedForm =
                    { form | rhesus = rhesusFromString value }
            in
            ( { model | bloodGpRsTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveBloodGpRsResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextTaskMsgs nextTask

                appMsgs =
                    toPrenatalBloodGpRsResultsValueWithDefault measurement model.bloodGpRsTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveBloodGpRsTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetProtein value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | protein = proteinValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetPH value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | ph = phValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetGlucose value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | glucose = glucoseValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetLeukocytes value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | leukocytes = leukocytesValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetNitrite value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | nitrite = nitriteValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetUrobilinogen value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | urobilinogen = urobilinogenValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetHaemoglobin value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | haemoglobin = haemoglobinValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetSpecificGravity value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | specificGravity = specificGravityValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetKetone value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | ketone = ketoneValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBilirubin value ->
            let
                form =
                    model.urineDipstickTestForm

                updatedForm =
                    { form | bilirubin = bilirubinValueFromString value }
            in
            ( { model | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveUrineDipstickResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextTaskMsgs nextTask

                appMsgs =
                    toPrenatalUrineDipstickResultsValueWithDefault measurement model.urineDipstickTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveUrineDipstickTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetHemoglobin value ->
            let
                form =
                    model.hemoglobinTestForm

                updatedForm =
                    { form | hemoglobinCount = String.toFloat value }
            in
            ( { model | hemoglobinTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveHemoglobinResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextTaskMsgs nextTask

                appMsgs =
                    toPrenatalHemoglobinResultsValueWithDefault measurement model.hemoglobinTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHemoglobinTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetRandomBloodSugar value ->
            let
                form =
                    model.randomBloodSugarTestForm

                updatedForm =
                    { form | sugarCount = String.toFloat value }
            in
            ( { model | randomBloodSugarTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveRandomBloodSugarResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextTaskMsgs nextTask

                appMsgs =
                    toPrenatalRandomBloodSugarResultsValueWithDefault measurement model.randomBloodSugarTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveRandomBloodSugarTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs
