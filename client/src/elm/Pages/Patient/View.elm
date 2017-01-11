module Pages.Patient.View exposing (view)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.Patient.Model exposing (Msg(..))
import Patient.Model exposing (PatientId, Patient)
import User.Model exposing (User)


view : Date -> User -> PatientId -> Patient -> Html Msg
view currentDate currentUser patientId patient =
    div []
        [ div
            [ class "ui secondary pointing fluid menu" ]
            [ h1
                [ class "ui header" ]
                [ text patient.name ]
            ]
        , div []
            [ img [ src patient.image ] []
            ]
        , div
            [ class "ui divider" ]
            []
        ]
