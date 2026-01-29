module Pages.Dashboard.Update exposing (update)

import App.Model
import Backend.Entities exposing (HealthCenterId)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.Dashboard.Model exposing (FilterPeriod(..), FilterProgramType(..), FilterType(..), Model, Msg(..), emptyModel, maxMonthGap)
import Pages.Dashboard.Utils exposing (filterProgramTypeFromString)
import Pages.Page exposing (DashboardPage(..), NutritionSubPage(..), Page(..), UserPage(..))
import Restful.Endpoint exposing (toEntityUuid)


update : NominalDate -> Maybe HealthCenterId -> DashboardPage -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate healthCenterId subPage db msg model =
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
            ( { model | currentCaseManagementSubFilter = filter }
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate healthCenterId subPage db)
                    [ SetActivePage (UserPage (DashboardPage (PageNutrition PageCaseManagement))) ]

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
            , getAssembledPermutationMsg healthCenterId updatedModel
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
            , getAssembledPermutationMsg healthCenterId updatedModel
            )

        SetActivePage page ->
            let
                -- When nurse navigates from Nutrition charts page to main page,
                -- reset filters to All Programs, unless village was selected.
                ( programTypeFilter, selectedVillageFilter ) =
                    case ( subPage, page ) of
                        ( PageNutrition PageCharts, UserPage (DashboardPage PageMain) ) ->
                            if model.programTypeFilter /= FilterProgramCommunity then
                                ( FilterAllPrograms, Nothing )

                            else
                                ( model.programTypeFilter, model.selectedVillageFilter )

                        _ ->
                            ( model.programTypeFilter, model.selectedVillageFilter )

                newPeriod =
                    case page of
                        UserPage (DashboardPage PageMain) ->
                            OneYear

                        UserPage (DashboardPage (PageNutrition PageStats)) ->
                            ThisMonth

                        UserPage (DashboardPage (PageNutrition PageCaseManagement)) ->
                            ThreeMonthsAgo

                        _ ->
                            OneYear
            in
            ( { model
                | latestPage = subPage
                , programTypeFilter = programTypeFilter
                , selectedVillageFilter = selectedVillageFilter
                , period = newPeriod
              }
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetEducationSessionDrillIn session ->
            ( { model | educationSessionDrillIn = session }
            , Cmd.none
            , []
            )


getAssembledPermutationMsg : Maybe HealthCenterId -> Model -> List App.Model.Msg
getAssembledPermutationMsg healthCenterId model =
    Maybe.map
        (\healthCenterId_ ->
            Backend.Model.FetchComputedDashboardAssembledPermutation healthCenterId_ model.programTypeFilter model.selectedVillageFilter
                |> App.Model.MsgIndexedDb
                |> List.singleton
        )
        healthCenterId
        |> Maybe.withDefault []
