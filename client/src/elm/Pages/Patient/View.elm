module Pages.Patient.View exposing (view)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.Patient.Model exposing (Msg(..))
import Patient.Model exposing (PatientId, Patient, PatientType(..))
import User.Model exposing (User)


view : Date -> User -> PatientId -> Patient -> Html Msg
view currentDate currentUser patientId patient =
    let
        -- @todo: move to Patient.View
        ( name, image ) =
            case patient.info of
                PatientChild child ->
                    ( child.name
                    , child.image
                    )

                PatientMother mother ->
                    ( mother.name
                    , mother.image
                    )
    in
        div []
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text name ]
                ]
            , div []
                [ img [ src image ] []
                ]
            , div
                [ class "ui divider" ]
                []
            ]
