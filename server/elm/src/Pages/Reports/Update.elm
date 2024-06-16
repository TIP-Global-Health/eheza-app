module Pages.Reports.Update exposing (update)

import App.Model exposing (PagesReturn)
import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Model exposing (PatientData)
import Date exposing (Interval(..), Unit(..))
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.Reports.Model exposing (..)
import Pages.Reports.Utils exposing (..)
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
                                ( Loading, performNutritionReportDataCalculation currentDate data.records )

                            _ ->
                                ( model.nutritionReportData, Cmd.none )

                modelUpdated =
                    { model | reportType = mReportType, nutritionReportData = nutritionReportData }
            in
            PagesReturn
                modelUpdated
                cmd
                noError
                []

        SetLimitDate value ->
            PagesReturn
                { model | limitDate = Just value }
                Cmd.none
                noError
                []

        SetLimitDateSelectorState state ->
            let
                defaultSelection =
                    Maybe.Extra.or model.limitDate (Maybe.andThen .dateDefault state)
            in
            PagesReturn
                { model | dateSelectorPopupState = state, limitDate = defaultSelection }
                Cmd.none
                noError
                []

        NutritionReportDataCalculationCompleted result ->
            PagesReturn
                { model | nutritionReportData = RemoteData.fromResult result }
                Cmd.none
                noError
                []


performNutritionReportDataCalculation : NominalDate -> List PatientData -> Cmd Msg
performNutritionReportDataCalculation currentDate data =
    Task.perform (Result.mapError (always "Calculations failed") >> NutritionReportDataCalculationCompleted)
        (wrapInResultTask <| calculateNutritionReportDataTask currentDate data)


calculateNutritionReportDataTask : NominalDate -> List PatientData -> Task String NutritionReportData
calculateNutritionReportDataTask currentDate data =
    Task.succeed
        (let
            records =
                List.filter
                    (\record ->
                        Date.diff Years record.birthDate currentDate < 6
                    )
                    data

            impacted =
                List.filterMap
                    (\record ->
                        if countTotalEncounetrs record > 1 then
                            Just record.id

                        else
                            Nothing
                    )
                    records

            currentYear =
                Date.year currentDate

            allEncounters =
                let
                    filterByYear encounter =
                        Date.year encounter.startDate >= startingYear

                    startingYear =
                        currentYear - 3
                in
                List.map
                    (\record ->
                        [ Maybe.map
                            (List.concat
                                >> List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.wellChildData
                        , Maybe.map
                            (List.concat
                                >> List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.individualNutritionData
                        , Maybe.map
                            (List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.groupNutritionPmtctData
                        , Maybe.map
                            (List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.groupNutritionFbfData
                        , Maybe.map
                            (List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.groupNutritionSorwatheData
                        , Maybe.map
                            (List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.groupNutritionChwData
                        , Maybe.map
                            (List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                            record.groupNutritionAchiData
                        ]
                            |> Maybe.Extra.values
                            |> List.concat
                    )
                    records
                    |> List.concat

            encountersByMonth =
                List.foldl
                    (\( personId, encounter ) accum ->
                        let
                            year =
                                Date.year encounter.startDate

                            month =
                                Date.monthNumber encounter.startDate

                            encounterMetrics =
                                nutritionEncounterDataToNutritionMetrics personId encounter

                            updatedMetrics =
                                Dict.get ( year, month ) accum
                                    |> Maybe.map
                                        (\metricsSoFar ->
                                            sumNutritionMetrics [ metricsSoFar, encounterMetrics ]
                                        )
                                    |> Maybe.withDefault encounterMetrics
                        in
                        Dict.insert ( year, month ) updatedMetrics accum
                    )
                    Dict.empty
                    allEncounters
         in
         { impacted = impacted
         , encountersByMonth = encountersByMonth
         }
        )


wrapInResultTask : Task String a -> Task Never (Result String a)
wrapInResultTask task =
    Task.map Result.Ok task
        |> Task.onError (Result.Err >> Task.succeed)