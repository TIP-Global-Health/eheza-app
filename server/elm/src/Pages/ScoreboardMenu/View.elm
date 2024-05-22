module Pages.ScoreboardMenu.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict
import Backend.Entities exposing (fromEntityId, toEntityId)
import Backend.Model exposing (ModelBackend)
import Backend.ScoreboardMenu.Model exposing (MenuData)
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isJust)
import Pages.ScoreboardMenu.Model exposing (..)
import Pages.Utils exposing (viewCustomLabel, viewGeoLocationSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (..)


view : Language -> ModelBackend -> Model -> Html Msg
view language modelBackend model =
    case modelBackend.scoreboardMenuData of
        Just (Ok data) ->
            viewMenu language data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewMenu : Language -> MenuData -> Model -> Html Msg
viewMenu language data model =
    let
        geoInfo =
            getGeoInfo data.site

        provinceInput =
            let
                options =
                    geoLocationDictToOptions geoInfo.provinces
            in
            viewGeoLocationSelectListInput language
                model.province
                options
                (SetGeoLocation
                    (\value form ->
                        { form
                            | province =
                                String.toInt value |> Maybe.map toEntityId
                        }
                    )
                )
                (resolveGeoSructureLabelLevel1 data.site)
                (isJust model.district)

        districtInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.districts
                                |> geoLocationDictToOptions
                    in
                    viewGeoLocationSelectListInput language
                        model.district
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | district = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel2 data.site)
                        (isJust model.sector)
                )
                model.province
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
                        model.sector
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | sector = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel3 data.site)
                        (isJust model.cell)
                )
                model.district
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
                        model.cell
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | cell = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel4 data.site)
                        (isJust model.village)
                )
                model.sector
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
                        model.village
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | village = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        (resolveGeoSructureLabelLevel5 data.site)
                        False
                )
                model.cell
                |> Maybe.withDefault emptyNode

        actionButton =
            Maybe.map2
                (\province district ->
                    if model.selected then
                        text <| translate language Translate.PleaseWaitMessage

                    else
                        let
                            provincePart =
                                Dict.get province geoInfo.provinces
                                    |> Maybe.map .name
                                    |> Maybe.withDefault ""

                            districtPart =
                                Dict.get district geoInfo.districts
                                    |> Maybe.map .name
                                    |> Maybe.withDefault ""

                            sectorPart =
                                Maybe.andThen
                                    (\id ->
                                        Dict.get id geoInfo.sectors
                                    )
                                    model.sector
                                    |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                                    |> Maybe.withDefault ""

                            cellPart =
                                Maybe.andThen
                                    (\id ->
                                        Dict.get id geoInfo.cells
                                    )
                                    model.cell
                                    |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                                    |> Maybe.withDefault ""

                            villagePart =
                                Maybe.andThen
                                    (\id ->
                                        Dict.get id geoInfo.villages
                                    )
                                    model.village
                                    |> Maybe.map (\geoLocation -> "/" ++ geoLocation.name)
                                    |> Maybe.withDefault ""

                            suffix =
                                provincePart
                                    ++ "/"
                                    ++ districtPart
                                    ++ sectorPart
                                    ++ cellPart
                                    ++ villagePart
                        in
                        a [ href <| "/admin/reports/aggregated-ncda/" ++ suffix ]
                            [ button [ onClick SelectionMade ]
                                [ text <| translate language Translate.GenerateReport ]
                            ]
                )
                model.province
                model.district
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-content" ] <|
        [ viewCustomLabel language Translate.SelectViewMode ":" "header"
        , div [ class "inputs" ]
            [ provinceInput
            , districtInput
            , sectorInput
            , cellInput
            , villageInput
            ]
        , div [ class "actions" ] [ actionButton ]
        ]
