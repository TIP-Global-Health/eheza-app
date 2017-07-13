module Measurement.View
    exposing
        ( view
        )

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Measurement.Model exposing (Model, Msg(..))
import Measurement.Utils exposing (getInputConstraintsWeight)
import Patient.Model exposing (Patient, PatientId)
import Translate as Trans exposing (Language(..), translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showMaybe)


view : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Maybe ActivityType -> Model -> Html Msg
view backendUrl accessToken user ( patientId, patient ) selectedActivity model =
    showMaybe <|
        (Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            Weight ->
                                viewWeight backendUrl accessToken user ( patientId, patient ) activity model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity
        )


viewWeight : BackendUrl -> String -> User -> ( PatientId, Patient ) -> ActivityType -> Model -> Html Msg
viewWeight backendUrl accessToken user ( patientId, patient ) selectedActivity model =
    let
        constraints =
            getInputConstraintsWeight

        language =
            English
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
                        ]
                        []
                    , span [] [ text <| translate language Trans.KilogramShorthand ]
                    ]
                ]
            ]
