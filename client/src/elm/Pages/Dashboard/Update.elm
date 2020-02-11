module Pages.Dashboard.Update exposing (update)

import App.Model
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))


update : Msg -> DashboardPage -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg subPage model =
    case msg of
        SetFilterGender gender ->
            ( { model | beneficiariesGender = gender }
            , Cmd.none
            , []
            )

        SetFilterPeriod period ->
            ( { model | period = period }
            , Cmd.none
            , []
            )

        SetFilterCaseManagement filter ->
            ( { model | currentCaseManagementFilter = filter }
            , Cmd.none
            , []
            )

        SetFilterTotalsChart filter ->
            ( { model | currentTotalChartsFilter = filter }
            , Cmd.none
            , []
            )

        SetActivePage page ->
            let
                newPeriod =
                    if page == UserPage (DashboardPage StatsPage) then
                        ThisMonth

                    else
                        OneYear
            in
            ( { model | latestPage = subPage, period = newPeriod }, Cmd.none, [ App.Model.SetActivePage page ] )
