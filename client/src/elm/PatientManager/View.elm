module PatientManager.View
    exposing
        ( viewPagePatient
        , viewPatients
        )

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Pages.Patient.View
import Pages.Patients.View
import Patient.Model exposing (PatientId, PatientsDict, PatientType(..))
import PatientManager.Model exposing (..)
import PatientManager.Utils exposing (getChildren, getMother, getPatient, unwrapPatientsDict)
import RemoteData exposing (RemoteData(..))
import User.Model exposing (User)
import Utils.WebData exposing (viewError)


{-| Show all Patients page.
-}
viewPatients : Date -> User -> Model -> Html Msg
viewPatients currentDate user model =
    let
        patients =
            unwrapPatientsDict model.patients
    in
        div []
            [ Html.map MsgPagesPatients <| Pages.Patients.View.view currentDate user patients model.patientsPage
            ]


{-| Show the Patient page.
-}
viewPagePatient : Date -> PatientId -> User -> Model -> Html Msg
viewPagePatient currentDate id user model =
    case getPatient id model of
        NotAsked ->
            -- This shouldn't happen, but if it does, we provide
            -- a button to load the editor
            div
                [ class "ui button"
                , onClick <| Subscribe id
                ]
                [ text "Re-load Patient" ]

        Loading ->
            div [] []

        Failure error ->
            div []
                [ viewError error
                , div
                    [ class "ui button"
                    , onClick <| Subscribe id
                    ]
                    [ text "Retry" ]
                ]

        Success patient ->
            case patient.info of
                PatientChild child ->
                    let
                        motherWebData =
                            getMother child.motherId model
                    in
                        div [] [ Html.map (MsgPagesPatient id) <| Pages.Patient.View.viewChild currentDate user id child motherWebData ]

                PatientMother mother ->
                    let
                        childrenWebData =
                            getChildren mother model
                    in
                        div [] [ Html.map (MsgPagesPatient id) <| Pages.Patient.View.viewMother currentDate user id mother childrenWebData ]
