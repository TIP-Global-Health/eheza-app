module Pages.Dashboard.Update exposing (update)

import App.Model
import Pages.Dashboard.Model exposing (..)
import Pages.Dashboard.Utils exposing (filterProgramTypeFromString)
import Pages.Page exposing (ChwDashboardPage(..), DashboardPage(..), NurseDashboardPage(..), Page(..), UserPage(..))
import Restful.Endpoint exposing (toEntityUuid)


update : Msg -> DashboardPage -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg subPage model =
    case msg of
        SetModalState state ->
            ( { model | modalState = state }
            , Cmd.none
            , []
            )

        Reset villageId ->
            ( emptyModel villageId
            , Cmd.none
            , []
            )

        ChangeMonthGap interval ->
            let
                potential =
                    model.monthGap + interval

                updated =
                    if potential < 0 || potential > maxMonthGap then
                        model.monthGap

                    else
                        potential
            in
            ( { model | monthGap = updated }
            , Cmd.none
            , []
            )

        NavigateToStuntingTable filter ->
            { model | currentCaseManagementSubFilter = filter }
                |> update (SetActivePage (UserPage (DashboardPage (NursePage CaseManagementPage)))) subPage

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

        SetSubFilterCaseManagement filter ->
            ( { model | currentCaseManagementSubFilter = filter }
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

        SetFilterProgramType string ->
            let
                updatedModel =
                    filterProgramTypeFromString string
                        |> Maybe.map
                            (\programTypeFilter ->
                                { model | programTypeFilter = programTypeFilter, selectedVillageFilter = Nothing }
                            )
                        |> Maybe.withDefault model
            in
            ( updatedModel
            , Cmd.none
            , []
            )

        SetSelectedVillage string ->
            let
                updatedModel =
                    if string == "" then
                        { model | selectedVillageFilter = Nothing }

                    else
                        { model | selectedVillageFilter = Just (toEntityUuid string) }
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

                        UserPage (DashboardPage (NursePage StatsPage)) ->
                            ThisMonth

                        UserPage (DashboardPage (NursePage CaseManagementPage)) ->
                            ThreeMonthsAgo

                        _ ->
                            OneYear
            in
            ( { model | latestPage = subPage, period = newPeriod }, Cmd.none, [ App.Model.SetActivePage page ] )
