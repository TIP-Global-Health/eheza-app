module Pages.Patients.View exposing (view)

import App.PageType exposing (Page(..))
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Pages.Patients.Model exposing (Model, Msg(..), PatientFilter(..))
import Patient.Model exposing (Patient, PatientId, PatientType(..), PatientsDict)
import Patient.Utils exposing (getPatientAvatarThumb, getPatientName)
import Table exposing (..)
import User.Model exposing (User)


view : Date -> User -> PatientsDict -> Model -> Html Msg
view currentDate currentUser patients model =
    let
        lowerQuery =
            String.toLower model.query

        acceptablePatients =
            Dict.filter
                (\patientId patient ->
                    let
                        validName =
                            String.contains lowerQuery (String.toLower <| getPatientName patient)

                        validType =
                            case model.patientFilter of
                                All ->
                                    True

                                Child ->
                                    case patient.info of
                                        PatientChild _ ->
                                            True

                                        _ ->
                                            False

                                Mother ->
                                    case patient.info of
                                        PatientMother _ ->
                                            True

                                        _ ->
                                            False
                    in
                        validName && validType
                )
                patients
                |> Dict.toList

        searchResult =
            if List.isEmpty acceptablePatients then
                if Dict.isEmpty patients then
                    -- No patients are present, so it means we are fethcing
                    -- them.
                    div [] []
                else
                    div [ class "ui segment" ] [ text "No patients found" ]
            else
                Table.view config model.tableState acceptablePatients
    in
        div []
            [ h1 [] [ text "Patients" ]
            , div [ class "ui input" ]
                [ input
                    [ placeholder "Search by Name"
                    , onInput SetQuery
                    ]
                    []
                , viewPatientFilter model
                ]
            , searchResult
            ]


viewPatientFilter : Model -> Html Msg
viewPatientFilter model =
    div []
        [ select
            [ class "ui dropdown"
              -- , value model.patientFilter
            ]
            [ option
                [ onClick <| SetPatientFilter All
                ]
                [ text "All" ]
            , option
                [ onClick <| SetPatientFilter Child
                ]
                [ text "Children" ]
            , option
                [ onClick <| SetPatientFilter Mother
                ]
                [ text "Mothers" ]
            ]
        ]


config : Table.Config ( PatientId, Patient ) Msg
config =
    Table.customConfig
        { toId = \( patientId, _ ) -> patientId
        , toMsg = SetTableState
        , columns =
            [ Table.veryCustomColumn
                { name = "Name"
                , viewData =
                    \( patientId, patient ) ->
                        Table.HtmlDetails []
                            [ a [ href "#", onClick <| SetRedirectPage <| App.PageType.Patient patientId ]
                                [ img [ src <| getPatientAvatarThumb patient ] []
                                , text <| getPatientName patient
                                ]
                            ]
                , sorter = Table.increasingOrDecreasingBy <| Tuple.second >> getPatientName
                }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "ui celled table" ] }
        }
