module Pages.Patients.View exposing (view)

import App.PageType exposing (Page(..))
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Pages.Patients.Model exposing (Model, Msg(..))
import Patient.Model exposing (Patient, PatientId, PatientType(..), PatientTypeFilter(..), PatientsDict)
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
                            case model.patientTypeFilter of
                                All ->
                                    True

                                Children ->
                                    case patient.info of
                                        PatientChild _ ->
                                            True

                                        _ ->
                                            False

                                Mothers ->
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
                , viewPatientTypeFilter model
                ]
            , searchResult
            ]


viewPatientTypeFilter : Model -> Html Msg
viewPatientTypeFilter model =
    div []
        [ select
            [ class "ui dropdown"
            , value <| toString model.patientTypeFilter
            , onInput SetPatientTypeFilter
            ]
            (List.map
                (\filterType ->
                    option
                        [ value <| toString filterType ]
                        [ text <| toString filterType ]
                )
                [ All, Children, Mothers ]
            )
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
                                [ img [ src <| getPatientAvatarThumb patient, class "ui avatar image" ] []
                                , text <| getPatientName patient
                                ]
                            ]
                , sorter = Table.increasingOrDecreasingBy <| Tuple.second >> getPatientName
                }
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "ui celled table" ] }
        }
