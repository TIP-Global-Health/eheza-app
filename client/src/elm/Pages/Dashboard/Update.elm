module Pages.Dashboard.Update exposing (update)

import App.Model
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
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
                updatedModel =
                    if page == UserPage (DashboardPage StatsPage) then
                        { model | period = ThisMonth }

                    else
                        { model | period = OneYear }
            in
            ( updatedModel, Cmd.none, [ App.Model.SetActivePage page ] )
