module Measurement.View
    exposing
        ( view
        )

import Activity.Model exposing (ChildActivityType(..))
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Measurement.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId)
import Translate as Trans exposing (Language, translate)
import Utils.Html exposing (emptyNode, showMaybe)
import User.Model exposing (..)


view : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Model -> Html Msg
view backendUrl accessToken user ( patientId, patient ) model =
    showMaybe <|
        (Maybe.map
            (\selectedActivity ->
                case selectedActivity of
                    Weight ->
                        viewWeight backendUrl accessToken user ( patientId, patient ) model

                    _ ->
                        emptyNode
            )
            model.selectedActivity
        )


viewWeight : BackendUrl -> String -> User -> ( PatientId, Patient ) -> Model -> Html Msg
viewWeight backendUrl accessToken user ( patientId, patient ) model =
    div [] [ text "Weight Form" ]
