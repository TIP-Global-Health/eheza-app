module Measurement.View
    exposing
        ( viewChild
        )

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Measurement.Model exposing (Model, Msg(..), getInputConstraintsWeight)
import Measurement.Utils exposing (isDirty)
import Child.Model exposing (Child, ChildId)
import Translate as Trans exposing (Language(..), translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showMaybe)


viewChild : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Maybe ActivityType -> Model -> Html Msg
viewChild backendUrl accessToken user language ( childId, child ) selectedActivity model =
    showMaybe <|
        (Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            Weight ->
                                viewWeight backendUrl accessToken user language ( childId, child ) model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity
        )



-- todo: Refactor this into Measurement module compliance after data loading is restored
-- priorWeight : Language -> Child -> Float -> Html Msg
-- priorWeight language child weight =
--     Maybe.map
--         (\value ->
--             let
--                 change =
--                     if value.weight < weight then
--                         Trans.MeasurementGained <| weight - value.weight
--                     else if value.weight > weight then
--                         Trans.MeasurementLost <| value.weight - weight
--                     else
--                         Trans.MeasurementNoChange
--             in
--                 div
--                     []
--                     [ span
--                         []
--                         [ text <| translate language (Trans.PriorWeight value.weight)
--                         ]
--                     , span
--                         []
--                         [ text <| translate language change
--                         ]
--                     ]
--         )
--         child.weight
--         |> showMaybe


viewWeight : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewWeight backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsWeight
    in
        div []
            [ divider
            , div
                [ class "ui card"
                ]
                [ h1
                    []
                    [ text <| translate language Trans.ActivitiesWeightTitle
                    ]
                , span
                    []
                    [ text <| translate language Trans.ActivitiesWeightHelp ]
                , div
                    []
                    [ span [] [ text <| translate language Trans.ActivitiesWeightLabel ]
                    , input
                        [ type_ "number"
                        , name "weight"
                        , step "0.5"
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        , value <| toString model.weight.value
                        , onInput
                            (\v ->
                                String.toFloat v
                                    |> Result.withDefault constraints.defaultValue
                                    |> clamp constraints.minVal constraints.maxVal
                                    |> WeightUpdate
                            )
                        ]
                        []
                    , span [] [ text <| translate language Trans.KilogramShorthand ]
                    ]
                , if isDirty constraints model.weight then
                    div [] [ text <| "*" ++ (translate language Trans.Save) ]
                  else
                    div [] [ text <| translate language Trans.Save ]
                ]
            ]
