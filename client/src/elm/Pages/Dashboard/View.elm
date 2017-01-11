module Pages.Dashboard.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Patient.Model exposing (Patient, PatientId)
import User.Model exposing (User)


view : User -> PatientsDict -> Html Msg
view currentUser patients =
    div []
        [ h1 [ class "ui header" ] [ text "Dashboard" ]
        , div [ class "ui divider" ] []
        , viewActiveIncidents patients
        ]


viewActiveIncidents : PatientsDict -> Html Msg
viewActiveIncidents patients =
    let
        orderedIncidentes =
            getOrderedIncidents patients
    in
        -- @todo: Filter out
        if (List.isEmpty orderedIncidentes) then
            div [ style [ ( "font-size", "300%" ) ] ]
                [ i [ class "ui icon check circle teal huge" ] []
                , text "No active incidents!"
                ]
        else
            div [ class "ui cards" ]
                (List.map
                    (\{ patientId, patient, incidentId, incident } ->
                        Html.map (MsgIncident patientId incidentId) (Incident.View.view ( patientId, patient ) ( incidentId, incident ) IncidentViewFull)
                    )
                    orderedIncidentes
                )
