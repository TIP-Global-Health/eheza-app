module Pages.Dashboard.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Patient.Model exposing (Patient, PatientId)
import Translated as Trans exposing (translate, Language)
import User.Model exposing (User)


view : Language -> User -> PatientsDict -> Html Msg
view language currentUser patients =
    div []
        [ h1 [ class "ui header" ] [ text <| translate language Trans.Dashboard ]
        , div [ class "ui divider" ] []
        , viewActiveIncidents language patients
        ]


viewActiveIncidents : Language -> PatientsDict -> Html Msg
viewActiveIncidents language patients =
    let
        orderedIncidentes =
            getOrderedIncidents patients
    in
        -- @todo: Filter out
        if (List.isEmpty orderedIncidentes) then
            div [ style [ ( "font-size", "300%" ) ] ]
                [ i [ class "ui icon check circle teal huge" ] []
                , text <| translate language Trans.NoActiveIncidents
                ]
        else
            div [ class "ui cards" ]
                (List.map
                    (\{ patientId, patient, incidentId, incident } ->
                        Html.map (MsgIncident patientId incidentId) (Incident.View.view language ( patientId, patient ) ( incidentId, incident ) IncidentViewFull)
                    )
                    orderedIncidentes
                )
