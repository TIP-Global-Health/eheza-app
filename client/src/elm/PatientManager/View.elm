module PatientManager.View
    exposing
        ( viewActivities
        , viewPagePatient
        , viewPatients
        )

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Pages.Activities.View
import Pages.Patient.View
import Pages.Patients.View
import Patient.Model exposing (PatientId, PatientsDict, PatientType(..), PatientTypeFilter(..))
import PatientManager.Model exposing (..)
import PatientManager.Utils exposing (getChildren, getMother, getPatient, unwrapPatientsDict)
import RemoteData exposing (RemoteData(..))
import Translate as Trans exposing (translate, Language)
import User.Model exposing (User)
import Utils.WebData exposing (viewError)


{-| Show all Patients page.
-}
viewPatients : Language -> Date -> User -> Model -> Html Msg
viewPatients language currentDate user model =
    let
        patients =
            unwrapPatientsDict model.patients
    in
        div []
            [ Html.map MsgPagesPatients <| Pages.Patients.View.view currentDate user patients model.patientsPage
            ]


{-| Show the Patient page.
-}
viewPagePatient : Language -> Date -> PatientId -> User -> Model -> Html Msg
viewPagePatient language currentDate id user model =
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
                        div [] [ Html.map (MsgPagesPatient id) <| Pages.Patient.View.viewChild language currentDate user id child motherWebData ]

                PatientMother mother ->
                    let
                        childrenWebData =
                            getChildren mother model
                    in
                        div [] [ Html.map (MsgPagesPatient id) <| Pages.Patient.View.viewMother language currentDate user id mother childrenWebData ]


viewActivities : Language -> Date -> User -> Model -> Html Msg
viewActivities language currentDate user model =
    let
        patients =
            unwrapPatientsDict model.patients
    in
        div []
            [ Html.map MsgPagesActivities <| Pages.Activities.View.view language currentDate user patients model.activitiesPage
            ]
