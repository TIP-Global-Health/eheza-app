module PatientManager.View
    exposing
        ( viewActivities
        , viewPagePatient
        , viewPatients
        )

import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Measurement.Model
import Pages.Activities.View
import Pages.Patient.Model
import Pages.Patient.View
import Pages.Patients.View
import Patient.Model exposing (PatientId, PatientType(..), PatientTypeFilter(..), PatientsDict)
import PatientManager.Model exposing (..)
import PatientManager.Utils exposing (getChildren, getMother, getPatient, unwrapPatientsDict)
import RemoteData exposing (RemoteData(..))
import Translate as Trans exposing (Language, translate)
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
            [ Html.map MsgPagesPatients <| Pages.Patients.View.view language currentDate user patients model.patientsPage
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
                [ text <| translate language Trans.ReloadPatient ]

        Loading ->
            div [] []

        Failure error ->
            div []
                [ viewError language error
                , div
                    [ class "ui button"
                    , onClick <| Subscribe id
                    ]
                    [ text <| translate language Trans.Retry ]
                ]

        Success patient ->
            let
                patientModel =
                    Maybe.map identity (Dict.get id model.patientPage)
                        |> Maybe.withDefault Pages.Patient.Model.emptyModel
            in
                case patient.info of
                    PatientChild child ->
                        let
                            motherWebData =
                                getMother child.motherId model

                            -- @todo: Wire
                            backendUrl =
                                "http://localhost:3000"

                            accessToken =
                                "dummy-access-token"

                            -- @todo: Remove duplication
                            patientModel =
                                Maybe.map identity (Dict.get id model.patientPage)
                                    |> Maybe.withDefault Pages.Patient.Model.emptyModel
                        in
                            div [] [ Html.map (MsgPagesPatient id) <| Pages.Patient.View.viewChild backendUrl accessToken user language currentDate motherWebData ( id, child ) patientModel ]

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
