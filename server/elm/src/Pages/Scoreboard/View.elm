module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language)
import Gizra.Html exposing (emptyNode, showIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Icons
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Scoreboard.Model exposing (..)
import Pages.Scoreboard.Utils exposing (..)
import Pages.Utils exposing (emptySelectOption, viewActionButton, viewLabel)
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (GeoLocationId, filterGeoLocationDictByParent, geoInfo, geoLocationDictToOptions)


view : Language -> Model -> Html Msg
view language model =
    case model.displayMode of
        DisplayViewSelection ->
            viewDisplayViewSelection language model

        DisplayResultTable value ->
            text "@todo viewDisplayResultTable"


viewDisplayViewSelection : Language -> Model -> Html Msg
viewDisplayViewSelection language model =
    let
        provinceInput =
            let
                options =
                    geoLocationDictToOptions geoInfo.provinces
            in
            viewSelectListInput language
                model.form.province
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
                (isJust model.form.district)

        districtInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.districts
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.district
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | district = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.District
                        (isJust model.form.sector)
                )
                model.form.province
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
                        model.form.sector
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | sector = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Sector
                        (isJust model.form.cell)
                )
                model.form.district
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
                        model.form.cell
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | cell = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Cell
                        (isJust model.form.village)
                )
                model.form.sector
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
                        model.form.village
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
                model.form.cell
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
        , viewActionButton language Translate.GenerateReport True GenerateReport
            |> showIf (isJust model.form.province && isJust model.form.district)
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
