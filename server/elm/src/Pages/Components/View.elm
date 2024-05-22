module Pages.Components.View exposing (..)

import App.Types exposing (Language, Site)
import AssocList as Dict
import Backend.Entities exposing (fromEntityId, toEntityId)
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isJust)
import Pages.Components.Model exposing (DemographicsSelection)
import Pages.ReportsMenu.Model exposing (..)
import Pages.ReportsMenu.Types exposing (..)
import Pages.ReportsMenu.Utils exposing (populationSelectionOptionToString)
import Pages.Utils exposing (viewCustomLabel, viewCustomSelectListInput, viewGeoLocationSelectListInput, viewSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (..)


viewDemographicsSelection :
    Language
    -> Site
    -> ((String -> DemographicsSelection -> DemographicsSelection) -> String -> msg)
    -> DemographicsSelection
    -> Html msg
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
    div [ class "inputs" ]
        [ provinceInput
        , districtInput
        , sectorInput
        , cellInput
        , villageInput
        ]


viewDemographicsSelectionActionButton : Language -> Site -> msg -> DemographicsSelection -> Html msg
viewDemographicsSelectionActionButton language site selectionMadeMsg selection =
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

        suffix =
            provincePart
                ++ districtPart
                ++ sectorPart
                ++ cellPart
                ++ villagePart
    in
    a [ href <| "/admin/reports/aggregated-ncda/" ++ suffix ]
        [ button [ onClick selectionMadeMsg ]
            [ text <| translate language Translate.GenerateReport ]
        ]
