module Pages.PrenatalLabResults.Update exposing (update)

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
import Pages.PrenatalLabResults.Model exposing (..)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetActiveTask task ->
            ( { model | activeTask = Just task }
            , Cmd.none
            , []
            )

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

        SetHemoglobinCount value ->
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
