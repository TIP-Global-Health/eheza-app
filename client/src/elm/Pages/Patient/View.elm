module Pages.Patient.View
    exposing
        ( view
        , viewChild
        , viewMother
        )

import Child.Model exposing (Child, ChildId)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Mother.Model exposing (Mother, MotherId)
import Pages.Patient.Model exposing (Msg(..))
import Patient.Model exposing (PatientId, Patient, PatientType(..))
import User.Model exposing (User)


view : Date -> User -> PatientId -> Patient -> Html Msg
view currentDate currentUser patientId patient =
    case patient.info of
        PatientChild child ->
            viewChild currentDate currentUser patientId child

        PatientMother mother ->
            viewMother currentDate currentUser patientId mother


viewChild : Date -> User -> ChildId -> Child -> Html Msg
viewChild currentDate currentUser childId child =
    div []
        [ div
            [ class "ui secondary pointing fluid menu" ]
            [ h1
                [ class "ui header" ]
                [ text child.name ]
            ]
        , div []
            [ img [ src child.image ] []
            ]
        , div
            [ class "ui divider" ]
            [ text <| "Mother ID: " ++ child.motherId ]
        ]


viewMother : Date -> User -> MotherId -> Mother -> Html Msg
viewMother currentDate currentUser motherId mother =
    div []
        [ div
            [ class "ui secondary pointing fluid menu" ]
            [ h1
                [ class "ui header" ]
                [ text mother.name ]
            ]
        , div []
            [ img [ src mother.image ] []
            ]
        , div
            [ class "ui divider" ]
            []
        ]
