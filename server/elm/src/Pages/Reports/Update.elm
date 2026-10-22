module Pages.Reports.Update exposing (update)

import App.Model exposing (PagesReturn)
import App.Ports
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Model exposing (PatientData)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Model exposing (Model, Msg(..), NutritionReportData, ReportType(..))
import Pages.Reports.Utils exposing (calculateNutritionReportData, isWideScope, reportTypeFromString)
import Pages.Utils exposing (dateSelectorDefault)
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)


update : NominalDate -> ModelBackend -> Msg -> Model -> PagesReturn Model Msg
update currentDate modelBackend msg model =
    case msg of
        SetReportType value ->
            let
                mReportType =
                    reportTypeFromString value

                ( nutritionReportData, cmd ) =
                    if RemoteData.isSuccess model.nutritionReportData then
                        ( model.nutritionReportData, Cmd.none )

                    else
                        case ( modelBackend.reportsData, mReportType ) of
                            ( Just (Ok data), Just ReportNutrition ) ->
                                -- For large data sets, nutrition report is generated on
                                -- backend. No need to generate proprietry data.
                                if isWideScope data.entityType then
                                    ( model.nutritionReportData, Cmd.none )

                                else
                                    ( Loading, performNutritionReportDataCalculation currentDate data.records )

                            _ ->
                                ( model.nutritionReportData, Cmd.none )

                modelUpdated =
                    { model
                        | reportType = mReportType
                        , startDate = Nothing
                        , limitDate = Nothing
                        , nutritionReportData = nutritionReportData
                    }
            in
            PagesReturn
                modelUpdated
                cmd
                noError
                []

        SetStartDate value ->
            PagesReturn
                { model | startDate = Just value }
                Cmd.none
                noError
                []

        SetStartDateSelectorState state ->
            PagesReturn
                { model | startDateSelectorPopupState = state, startDate = dateSelectorDefault model.startDate state }
                Cmd.none
                noError
                []

        SetLimitDate value ->
            PagesReturn
                { model | limitDate = Just value }
                Cmd.none
                noError
                []

        SetLimitDateSelectorState state ->
            PagesReturn
                { model | limitDateSelectorPopupState = state, limitDate = dateSelectorDefault model.limitDate state }
                Cmd.none
                noError
                []

        NutritionReportDataCalculationCompleted result ->
            PagesReturn
                { model | nutritionReportData = RemoteData.fromResult result }
                Cmd.none
                noError
                []

        DownloadCSV fileName content ->
            PagesReturn
                model
                (App.Ports.downloadCsv ( fileName, content ))
                noError
                []


performNutritionReportDataCalculation : NominalDate -> List PatientData -> Cmd Msg
performNutritionReportDataCalculation currentDate data =
    Task.perform (Result.mapError (always "Calculations failed") >> NutritionReportDataCalculationCompleted)
        (wrapInResultTask <| calculateNutritionReportDataTask currentDate data)


calculateNutritionReportDataTask : NominalDate -> List PatientData -> Task String NutritionReportData
calculateNutritionReportDataTask currentDate data =
    Task.succeed
        (calculateNutritionReportData currentDate data)


wrapInResultTask : Task String a -> Task Never (Result String a)
wrapInResultTask task =
    Task.map Result.Ok task
        |> Task.onError (Result.Err >> Task.succeed)
