module Pages.Menu.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Date
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Menu.Model exposing (..)
import Pages.Utils exposing (emptySelectOption, viewActionButton, viewLabel)
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (GeoLocationId, filterGeoLocationDictByParent, geoInfo, geoLocationDictToOptions)


view : Language -> Model -> Html Msg
view language model =
    let
        provinceInput =
            let
                options =
                    geoLocationDictToOptions geoInfo.provinces
            in
            viewSelectListInput language
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
                Translate.Province
                (isJust model.district)

        districtInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.districts
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.district
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | district = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.District
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
                    viewSelectListInput language
                        model.sector
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | sector = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Sector
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
                    viewSelectListInput language
                        model.cell
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | cell = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Cell
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
                    viewSelectListInput language
                        model.village
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | village = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Village
                        False
                )
                model.cell
                |> Maybe.withDefault emptyNode

        actionButton =
            Maybe.map2
                (\province district ->
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
                    div [ class "actions" ]
                        [ a [ href <| "/admin/reports/aggregated-ncda/" ++ suffix ]
                            [ button [] [ text <| translate language Translate.GenerateReport ] ]
                        ]
                )
                model.province
                model.district
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-content" ] <|
        [ div [ class "header" ] [ text "Please select desired view mode:" ]
        , div [ class "inputs" ]
            [ provinceInput
            , districtInput
            , sectorInput
            , cellInput
            , villageInput
            ]
        , actionButton
        ]


viewSelectListInput :
    Language
    -> Maybe GeoLocationId
    -> List ( String, String )
    -> (String -> Msg)
    -> TranslationId
    -> Bool
    -> Html Msg
viewSelectListInput language currentValue options setMsg labelTransId disabled =
    let
        selectOptions =
            emptyOption
                :: List.map
                    (\option_ ->
                        let
                            isSelected =
                                Tuple.first option_
                                    |> String.toInt
                                    |> Maybe.map
                                        (\id ->
                                            currentValue == (Just <| toEntityId id)
                                        )
                                    |> Maybe.withDefault False
                        in
                        option
                            [ value <| Tuple.first option_
                            , selected isSelected
                            ]
                            [ text <| Tuple.second option_ ]
                    )
                    options

        emptyOption =
            emptySelectOption (currentValue == Nothing)
    in
    div
        [ classList
            [ ( "select-input-wrapper", True )
            , ( "disabled", disabled )
            ]
        ]
        [ viewLabel language labelTransId
        , select
            [ onInput setMsg
            , class "select-input"
            ]
            selectOptions
        ]
