module Pages.Reports.Update exposing (update)

import App.Model exposing (PagesReturn)
import App.Ports
import App.Types exposing (SiteFeature(..))
import AssocList as Dict
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Model exposing (PatientData)
import Date exposing (Unit(..))
import Error.Utils exposing (noError)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.Reports.Model exposing (Model, Msg(..), NutritionReportData, ReportType(..))
import Pages.Reports.Utils exposing (countTotalNutritionEncounters, familyNutritionEncounterToMetrics, isWideScope, nutritionEncounterDataToNutritionMetrics, reportTypeFromString, sumNutritionMetrics)
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
                                    ( Loading, performNutritionReportDataCalculation currentDate data.features data.records )

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
            let
                defaultSelection =
                    Maybe.Extra.or model.startDate (Maybe.andThen .dateDefault state)
            in
            PagesReturn
                { model | startDateSelectorPopupState = state, startDate = defaultSelection }
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
            let
                defaultSelection =
                    Maybe.Extra.or model.limitDate (Maybe.andThen .dateDefault state)
            in
            PagesReturn
                { model | limitDateSelectorPopupState = state, limitDate = defaultSelection }
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


performNutritionReportDataCalculation : NominalDate -> EverySet SiteFeature -> List PatientData -> Cmd Msg
performNutritionReportDataCalculation currentDate features data =
    Task.perform (Result.mapError (always "Calculations failed") >> NutritionReportDataCalculationCompleted)
        (wrapInResultTask <| calculateNutritionReportDataTask currentDate features data)


calculateNutritionReportDataTask : NominalDate -> EverySet SiteFeature -> List PatientData -> Task String NutritionReportData
calculateNutritionReportDataTask currentDate features data =
    Task.succeed
        (let
            memberFeature f =
                EverySet.member f features

            wellChildEnabled =
                memberFeature FeatureWellChild

            nutritionIndividualEnabled =
                memberFeature FeatureNutritionIndividual

            nutritionGroupEnabled =
                memberFeature FeatureNutritionGroup

            familyNutritionEnabled =
                memberFeature FeatureFamilyNutrition

            records =
                List.filter
                    (\record ->
                        Date.diff Years record.birthDate currentDate < 6
                    )
                    data

            impacted =
                List.filterMap
                    (\record ->
                        if countTotalNutritionEncounters record > 1 then
                            Just record.id

                        else
                            Nothing
                    )
                    records

            currentYear =
                Date.year currentDate

            startingYear =
                currentYear - 3

            filterByYear encounter =
                Date.year encounter.startDate >= startingYear

            seriesIf flag series =
                if flag then
                    series

                else
                    Nothing

            allEncounters =
                List.concatMap
                    (\record ->
                        [ seriesIf wellChildEnabled <|
                            Maybe.map
                                (List.concat
                                    >> List.filter filterByYear
                                    >> List.map
                                        (\item ->
                                            -- WellChildEncounterData mirrors NutritionEncounterData's
                                            -- shape (date, nutritionData, muacCm) plus immunisationData;
                                            -- well-child wire doesn't carry edema or FBF distribution
                                            -- today, so default both to absent.
                                            ( record.id
                                            , { startDate = item.startDate
                                              , nutritionData = item.nutritionData
                                              , muacCm = item.muacCm
                                              , hasEdema = False
                                              , fbfAmount = Nothing
                                              }
                                            )
                                        )
                                )
                                record.wellChildData
                        , seriesIf nutritionIndividualEnabled <|
                            Maybe.map
                                (List.concat
                                    >> List.filter filterByYear
                                    >> List.map (Tuple.pair record.id)
                                )
                                record.individualNutritionData
                        , seriesIf nutritionGroupEnabled <|
                            Maybe.map
                                (List.filter filterByYear
                                    >> List.map (Tuple.pair record.id)
                                )
                                record.groupNutritionPmtctData
                        , seriesIf nutritionGroupEnabled <|
                            Maybe.map
                                (List.filter filterByYear
                                    >> List.map (Tuple.pair record.id)
                                )
                                record.groupNutritionFbfData
                        , seriesIf nutritionGroupEnabled <|
                            Maybe.map
                                (List.filter filterByYear
                                    >> List.map (Tuple.pair record.id)
                                )
                                record.groupNutritionSorwatheData
                        , seriesIf nutritionGroupEnabled <|
                            Maybe.map
                                (List.filter filterByYear
                                    >> List.map (Tuple.pair record.id)
                                )
                                record.groupNutritionChwData
                        , seriesIf nutritionGroupEnabled <|
                            Maybe.map
                                (List.filter filterByYear
                                    >> List.map (Tuple.pair record.id)
                                )
                                record.groupNutritionAchiData
                        ]
                            |> Maybe.Extra.values
                            |> List.concat
                    )
                    records

            -- Per-child family-nutrition MUAC measurements live under
            -- familyNutritionMuacData on each child record. The bucket
            -- entry uses record.id (the child's id), giving correct
            -- per-child denominators in the AM stats. The mother-side
            -- familyNutritionData (date-only entries used by the
            -- Demographics encounter row) is intentionally not consumed
            -- here -- mothers are filtered out by the records age filter
            -- above, and the Demographics row already counts them
            -- elsewhere.
            familyNutritionEncounters =
                if familyNutritionEnabled then
                    List.concatMap
                        (\record ->
                            record.familyNutritionMuacData
                                |> Maybe.map
                                    (List.concat
                                        >> List.filter filterByYear
                                        >> List.map (Tuple.pair record.id)
                                    )
                                |> Maybe.withDefault []
                        )
                        records

                else
                    []

            insertMetricsForMonth ( year, month ) encounterMetrics accum =
                let
                    updatedMetrics =
                        Dict.get ( year, month ) accum
                            |> Maybe.map
                                (\metricsSoFar ->
                                    sumNutritionMetrics [ metricsSoFar, encounterMetrics ]
                                )
                            |> Maybe.withDefault encounterMetrics
                in
                Dict.insert ( year, month ) updatedMetrics accum

            encountersByMonthFromNutrition =
                List.foldl
                    (\( personId, encounter ) accum ->
                        let
                            year =
                                Date.year encounter.startDate

                            month =
                                Date.monthNumber encounter.startDate

                            encounterMetrics =
                                nutritionEncounterDataToNutritionMetrics personId encounter
                        in
                        insertMetricsForMonth ( year, month ) encounterMetrics accum
                    )
                    Dict.empty
                    allEncounters

            encountersByMonth =
                List.foldl
                    (\( personId, encounter ) accum ->
                        let
                            year =
                                Date.year encounter.startDate

                            month =
                                Date.monthNumber encounter.startDate

                            encounterMetrics =
                                familyNutritionEncounterToMetrics personId encounter
                        in
                        insertMetricsForMonth ( year, month ) encounterMetrics accum
                    )
                    encountersByMonthFromNutrition
                    familyNutritionEncounters
         in
         { impacted = impacted
         , encountersByMonth = encountersByMonth
         }
        )


wrapInResultTask : Task String a -> Task Never (Result String a)
wrapInResultTask task =
    Task.map Result.Ok task
        |> Task.onError (Result.Err >> Task.succeed)
