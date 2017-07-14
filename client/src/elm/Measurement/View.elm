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
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        , value <| toString model.weight.value
                        , onInput <| (\v -> WeightUpdate <| Result.withDefault 0.0 <| String.toFloat v)
                        ]
                        []
                    , span [] [ text <| translate language Trans.KilogramShorthand ]
                    ]
                ]
            ]
