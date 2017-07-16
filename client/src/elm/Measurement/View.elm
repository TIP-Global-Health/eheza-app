module Measurement.View
    exposing
        ( viewChild
        )

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Measurement.Model exposing (Model, Msg(..), getInputConstraintsHeight, getInputConstraintsMuac, getInputConstraintsWeight)
import RemoteData exposing (RemoteData(..), isFailure, isLoading)
import Translate as Trans exposing (Language(..), translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showIf, showMaybe)


viewChild : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Maybe ActivityType -> Model -> Html Msg
viewChild backendUrl accessToken user language ( childId, child ) selectedActivity model =
    showMaybe <|
        (Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            Height ->
                                viewHeight backendUrl accessToken user language ( childId, child ) model

                            Muac ->
                                viewMuac backendUrl accessToken user language ( childId, child ) model

                            Weight ->
                                viewWeight backendUrl accessToken user language ( childId, child ) model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity
        )


viewHeight : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewHeight backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsHeight
    in
        div []
            [ divider
            , div
                [ class "ui card height"
                ]
                [ h1
                    []
                    [ text <| translate language Trans.ActivitiesHeightTitle
                    ]
                , span
                    []
                    [ text <| translate language Trans.ActivitiesHeightHelp ]
                , div
                    []
                    [ span [] [ text <| translate language Trans.ActivitiesHeightLabel ]
                    , input
                        [ type_ "number"
                        , name "height"
                        , step "0.5"
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        , value <| toString model.height.value
                        , onInput
                            (\v ->
                                String.toFloat v
                                    |> Result.withDefault constraints.defaultValue
                                    |> clamp constraints.minVal constraints.maxVal
                                    |> HeightUpdate
                            )
                        ]
                        []
                    , span [] [ text <| translate language Trans.CentimeterShorthand ]
                    ]
                , saveButon language HeightSave model
                ]
            ]


viewMuac : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewMuac backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsMuac
    in
        div []
            [ divider
            , div
                [ class "ui card muac"
                ]
                [ h1
                    []
                    [ text <| translate language Trans.ActivitiesMuacTitle
                    ]
                , span
                    []
                    [ text <| translate language Trans.ActivitiesMuacHelp ]
                , div
                    []
                    [ span [] [ text <| translate language Trans.ActivitiesMuacLabel ]
                    , input
                        [ type_ "number"
                        , name "muac"
                        , step "0.5"
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        , value <| toString model.muac.value
                        , onInput
                            (\v ->
                                String.toFloat v
                                    |> Result.withDefault constraints.defaultValue
                                    |> clamp constraints.minVal constraints.maxVal
                                    |> MuacUpdate
                            )
                        ]
                        []
                    , span [] [ text <| translate language Trans.CentimeterShorthand ]
                    ]
                , saveButon language MuacSave model
                ]
            ]


viewWeight : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewWeight backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsWeight
    in
        div []
            [ divider
            , div
                [ class "ui segment"
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
                , saveButon language WeightSave model
                ]
            ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submisson,
and showing success and error indications.

-}
saveButon : Language -> Msg -> Model -> Html Msg
saveButon language msg model =
    let
        isLoading =
            model.status == Loading

        isSuccess =
            RemoteData.isSuccess model.status

        isFailure =
            RemoteData.isFailure model.status

        saveAttr =
            if isLoading then
                []
            else
                [ onClick msg ]
    in
        div []
            [ div
                ([ classList
                    [ ( "ui button primary", True )
                    , ( "loading", isLoading )
                    , ( "positive", isSuccess )
                    , ( "negative", isFailure )
                    ]
                 ]
                    ++ saveAttr
                )
                [ text <| translate language Trans.Save
                ]
            , showIf isFailure <| div [] [ text <| translate language Trans.SaveError ]
            ]
