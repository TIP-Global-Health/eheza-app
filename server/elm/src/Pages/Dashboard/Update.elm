module Pages.Dashboard.Update exposing (update)

import App.Model exposing (PagesReturn)
import App.Ports
import AssocList as Dict
import Error.Utils exposing (noError)
import Pages.Dashboard.Model exposing (Model, Msg(..), Screen(..))


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        CloseDrill ->
            PagesReturn
                { model | screen = DashboardScreen }
                Cmd.none
                noError
                []

        DownloadCSV fileName content ->
            PagesReturn
                model
                (App.Ports.downloadCsv ( fileName, content ))
                noError
                []

        OpenDrill kpi ->
            PagesReturn
                { model | screen = DrillScreen kpi }
                Cmd.none
                noError
                []

        PrintPage ->
            PagesReturn
                model
                (App.Ports.printPage ())
                noError
                []

        SelectTrendKpi kpiId ->
            PagesReturn
                { model | trendKpi = Just kpiId }
                Cmd.none
                noError
                []

        SelectTrendYear year ->
            PagesReturn
                { model | trendYear = year }
                Cmd.none
                noError
                []

        SetFilter key value ->
            PagesReturn
                { model | selectedFilters = Dict.insert key value model.selectedFilters }
                Cmd.none
                noError
                []
