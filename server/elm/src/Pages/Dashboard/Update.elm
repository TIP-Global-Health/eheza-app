module Pages.Dashboard.Update exposing (update)

import App.Model exposing (PagesReturn)
import App.Ports
import AssocList as Dict
import Browser.Dom
import Error.Utils exposing (noError)
import Pages.Dashboard.Model exposing (Model, Msg(..), Screen(..))
import Pages.Dashboard.Utils exposing (drillDialogId, kpiBlockId)
import Task


update : Msg -> Model -> PagesReturn Model Msg
update msg model =
    case msg of
        CloseDrill ->
            PagesReturn
                { model | screen = DashboardScreen }
                (case model.screen of
                    DashboardScreen ->
                        Cmd.none

                    DrillScreen kpi ->
                        focus (kpiBlockId kpi)
                )
                noError
                []

        DownloadCSV fileName content ->
            PagesReturn
                model
                (App.Ports.downloadCsv ( fileName, content ))
                noError
                []

        NoOp ->
            PagesReturn model Cmd.none noError []

        OpenDrill kpi ->
            PagesReturn
                { model | screen = DrillScreen kpi }
                (focus drillDialogId)
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


{-| Move focus to an element, if it is there to take it.
-}
focus : String -> Cmd Msg
focus elementId =
    Browser.Dom.focus elementId
        |> Task.attempt (always NoOp)
