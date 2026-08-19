module Pages.Components.View exposing (viewDemographicsSelection, viewDemographicsSelectionActionButton, viewHealthCenterSelection, viewMetricsResultsTable, viewPopulationSelectionInput, viewReportDateInputs, viewStandardCells, viewStandardRow)

import App.Types exposing (Language, Site)
import AssocList as Dict
import Backend.Components.Model exposing (HealthCenterId, MenuData, MenuScope(..))
import Backend.Entities exposing (fromEntityId, toEntityId)
import DateSelector.Model exposing (DateSelectorConfig)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Components.Model exposing (DemographicsSelection)
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Components.Utils exposing (populationSelectionOptionToString)
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils
    exposing
        ( launchDate
        , viewCustomSelectListInput
        , viewGeoLocationSelectListInput
        , viewLoadDataButton
        , viewMenuActionButton
        , viewSelectListInput
        , wrapSelectListInput
        )
import Translate exposing (TranslationId)
import Utils.GeoLocation exposing (..)


viewPopulationSelectionInput :
    Language
    -> MenuData
    -> List PopulationSelectionOption
    -> Maybe PopulationSelectionOption
    -> (String -> msg)
    -> Html msg
viewPopulationSelectionInput language data allOptions populationSelection setPopulationSelectionMsg =
    let
        options =
            Maybe.map
                (\scope ->
                    case scope of
                        ScopeFull ->
                            allOptions

                        ScopeHealthCenters ->
                            [ SelectionOptionHealthCenter ]
                )
                data.scope
                |> Maybe.withDefault allOptions
    in
    viewSelectListInput language
        populationSelection
        options
        populationSelectionOptionToString
        setPopulationSelectionMsg
        Translate.PopulationSelectionOption
        "select-input"
        |> wrapSelectListInput language Translate.Scope False


{-| The health-center scope inputs of a menu page - the health-center
select list, and the action button that leads to the page for the
selected health center (its URL is the given prefix followed by the
health center ID).
-}
viewHealthCenterSelection :
    Language
    -> MenuData
    -> String
    -> Maybe HealthCenterId
    -> (String -> msg)
    -> msg
    -> ( List (Html msg), Html msg )
viewHealthCenterSelection language data urlPrefix selectedHealthCenter setHealthCenterMsg selectionMadeMsg =
    let
        options =
            List.sortBy .name data.healthCenters
                |> List.map (\healthCenter -> ( healthCenter.name, healthCenter.id ))
    in
    ( [ viewCustomSelectListInput
            selectedHealthCenter
            options
            String.fromInt
            setHealthCenterMsg
            "select-input"
            (Just "")
            |> wrapSelectListInput language Translate.HealthCenter False
      ]
    , Maybe.map
        (\healthCenterId ->
            viewLoadDataButton language
                (urlPrefix ++ String.fromInt healthCenterId)
                selectionMadeMsg
        )
        selectedHealthCenter
        |> Maybe.withDefault emptyNode
    )


viewReportDateInputs :
    Language
    -> NominalDate
    -> Maybe NominalDate
    -> Maybe NominalDate
    -> (NominalDate -> msg)
    -> (Maybe (DateSelectorConfig msg) -> msg)
    -> (NominalDate -> msg)
    -> (Maybe (DateSelectorConfig msg) -> msg)
    -> List (Html msg)
viewReportDateInputs language currentDate startDate limitDate setStartDateMsg setStartDateSelectorStateMsg setLimitDateMsg setLimitDateSelectorStateMsg =
    let
        startDateInput =
            let
                dateSelectorConfig =
                    { select = setStartDateMsg
                    , close = setStartDateSelectorStateMsg Nothing
                    , dateFrom = launchDate
                    , dateTo = currentDate
                    , dateDefault = Just launchDate
                    }

                dateForView =
                    Maybe.map formatDDMMYYYY startDate
                        |> Maybe.withDefault ""
            in
            div
                [ class "form-input date"
                , onClick <| setStartDateSelectorStateMsg (Just dateSelectorConfig)
                ]
                [ text dateForView ]
                |> wrapSelectListInput language Translate.SelectStartDate False

        limitDateInput =
            if
                -- Reports requires setting start date before
                -- limit date can be shown.
                isNothing startDate
            then
                emptyNode

            else
                let
                    dateFrom =
                        Maybe.withDefault launchDate startDate

                    dateSelectorConfig =
                        { select = setLimitDateMsg
                        , close = setLimitDateSelectorStateMsg Nothing
                        , dateFrom = dateFrom
                        , dateTo = currentDate
                        , dateDefault = Just currentDate
                        }

                    limitDateForView =
                        Maybe.map formatDDMMYYYY limitDate
                            |> Maybe.withDefault ""
                in
                div
                    [ class "form-input date"
                    , onClick <| setLimitDateSelectorStateMsg (Just dateSelectorConfig)
                    ]
                    [ text limitDateForView ]
                    |> wrapSelectListInput language Translate.SelectLimitDate False
    in
    [ startDateInput, limitDateInput ]


viewDemographicsSelection :
    Language
    -> Site
    -> ((String -> DemographicsSelection -> DemographicsSelection) -> String -> msg)
    -> DemographicsSelection
    -> List (Html msg)
viewDemographicsSelection language site setGeoLocationMsg selection =
    let
        geoInfo =
            getGeoInfo site

        provinceInput =
            let
                options =
                    geoLocationDictToOptions geoInfo.provinces
            in
            viewGeoLocationSelectListInput language
                selection.province
                options
                (setGeoLocationMsg
                    (\value form ->
                        { form
                            | province =
                                String.toInt value |> Maybe.map toEntityId
                        }
                    )
                )
                (resolveGeoSructureLabelLevel1 site)
                (isJust selection.district)

        districtInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.districts
                                |> geoLocationDictToOptions
                    in
                    viewGeoLocationSelectListInput language
                        selection.district
                        options
                        (setGeoLocationMsg
                            (\value form ->
                                { form
                                    | district = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel2 site)
                        (isJust selection.sector)
                )
                selection.province
                |> Maybe.withDefault emptyNode

        sectorInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.sectors
                                |> geoLocationDictToOptions
                    in
                    viewGeoLocationSelectListInput language
                        selection.sector
                        options
                        (setGeoLocationMsg
                            (\value form ->
                                { form
                                    | sector = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel3 site)
                        (isJust selection.cell)
                )
                selection.district
                |> Maybe.withDefault emptyNode

        cellInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.cells
                                |> geoLocationDictToOptions
                    in
                    viewGeoLocationSelectListInput language
                        selection.cell
                        options
                        (setGeoLocationMsg
                            (\value form ->
                                { form
                                    | cell = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel4 site)
                        (isJust selection.village)
                )
                selection.sector
                |> Maybe.withDefault emptyNode

        villageInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.villages
                                |> geoLocationDictToOptions
                    in
                    viewGeoLocationSelectListInput language
                        selection.village
                        options
                        (setGeoLocationMsg
                            (\value form ->
                                { form
                                    | village = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel5 site)
                        False
                )
                selection.cell
                |> Maybe.withDefault emptyNode
    in
    [ provinceInput
    , districtInput
    , sectorInput
    , cellInput
    , villageInput
    ]


viewDemographicsSelectionActionButton : Language -> Site -> String -> TranslationId -> msg -> DemographicsSelection -> Html msg
viewDemographicsSelectionActionButton language site pathPrefix label selectionMadeMsg selection =
    let
        geoInfo =
            getGeoInfo site

        provincePart =
            Maybe.andThen
                (\id ->
                    Dict.get id geoInfo.provinces
                )
                selection.province
                |> Maybe.map .name
                |> Maybe.withDefault ""

        districtPart =
            Maybe.andThen
                (\id ->
                    Dict.get id geoInfo.districts
                )
                selection.district
                |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                |> Maybe.withDefault ""

        sectorPart =
            Maybe.andThen
                (\id ->
                    Dict.get id geoInfo.sectors
                )
                selection.sector
                |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                |> Maybe.withDefault ""

        cellPart =
            Maybe.andThen
                (\id ->
                    Dict.get id geoInfo.cells
                )
                selection.cell
                |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                |> Maybe.withDefault ""

        villagePart =
            Maybe.andThen
                (\id ->
                    Dict.get id geoInfo.villages
                )
                selection.village
                |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                |> Maybe.withDefault ""

        path =
            pathPrefix
                ++ "/"
                ++ provincePart
                ++ districtPart
                ++ sectorPart
                ++ cellPart
                ++ villagePart
    in
    viewMenuActionButton language path label selectionMadeMsg



-- Table


viewMetricsResultsTable : MetricsResultsTableData -> List (Html any)
viewMetricsResultsTable data =
    let
        captionsRow =
            div [ class "row" ] <|
                viewCustomCells "row-label" "heading" data.captions

        viewRow cells =
            div [ class "row" ] <|
                viewCustomCells "row-label" "value" cells
    in
    [ div [ class "section heading" ] [ text data.heading ]
    , div [ class "table wide" ] <|
        captionsRow
            :: List.map viewRow data.rows
    ]


viewStandardRow : List String -> Html any
viewStandardRow =
    viewStandardCells
        >> div [ class "row" ]


viewStandardCells : List String -> List (Html any)
viewStandardCells =
    viewCustomCells "label" "value"


viewCustomCells : String -> String -> List String -> List (Html any)
viewCustomCells labelClass valueClass =
    List.indexedMap
        (\index cellText ->
            div
                [ classList
                    [ ( "item", True )
                    , ( labelClass, index == 0 )
                    , ( valueClass, index /= 0 )
                    ]
                ]
                [ text cellText ]
        )
