module Pages.Dashboard.Update exposing (update)

import App.Model
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))


update : Msg -> DashboardPage -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg subPage model =
    case msg of
        ModalToggle state table title ->
            ( { model | modalTable = table, modalState = state, modalTitle = title }
            , Cmd.none
            , []
            )

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

        SetFilterBeneficiariesChart filter filterType ->
            let
                updatedModel =
                    if filterType == FilterBeneficiariesChart then
                        { model | currentBeneficiariesChartsFilter = filter }

                    else
                        { model | currentBeneficiariesIncidenceChartsFilter = filter }
            in
            ( updatedModel
            , Cmd.none
            , []
            )

        SetActivePage page ->
            let
                newPeriod =
                    case page of
                        UserPage (DashboardPage MainPage) ->
                            OneYear

                        UserPage (DashboardPage StatsPage) ->
                            ThisMonth

                        UserPage (DashboardPage CaseManagementPage) ->
                            ThreeMonthsAgo

                        _ ->
                            OneYear
            in
            ( { model | latestPage = subPage, period = newPeriod }, Cmd.none, [ App.Model.SetActivePage page ] )
