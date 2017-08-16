module Pages.Dashboard.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Participant.Model exposing (Participant, ParticipantId)
import Translated as Trans exposing (translate, Language)
import User.Model exposing (User)


view : Language -> User -> ParticipantsDict -> Html Msg
view language currentUser participants =
    div []
        [ h1 [ class "ui header" ] [ text <| translate language Trans.Dashboard ]
        , div [ class "ui divider" ] []
        , viewActiveIncidents language participants
        ]


viewActiveIncidents : Language -> ParticipantsDict -> Html Msg
viewActiveIncidents language participants =
    let
        orderedIncidentes =
            getOrderedIncidents participants
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
                    (\{ participantId, participant, incidentId, incident } ->
                        Html.map (MsgIncident participantId incidentId) (Incident.View.view language ( participantId, participant ) ( incidentId, incident ) IncidentViewFull)
                    )
                    orderedIncidentes
                )
