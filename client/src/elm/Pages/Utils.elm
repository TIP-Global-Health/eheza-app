module Pages.Utils exposing (..)

{-| Some UI-related code used in multiple modules.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..))
import Translate exposing (translate, Language)


type DashboardPage
    = ActivitiesDashboard
    | ParticipantsDashboard


viewDashboardPageHeader : (Page -> msg) -> Language -> DashboardPage -> Html msg
viewDashboardPageHeader redirect language dashboardPage =
    let
        ( header, activitiesLinkAttributes, participantsLinkAttributes ) =
            case dashboardPage of
                ActivitiesDashboard ->
                    ( Translate.Activities
                    , [ class "active" ]
                    , [ onClick (redirect (Debug.crash "where to"))
                      ]
                    )

                ParticipantsDashboard ->
                    ( Translate.Participants
                    , [ onClick (redirect ActivitiesPage) ]
                    , [ class "active" ]
                    )
    in
        div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language header ]
            , a
                [ class "link-back" ]
                [ span [ class "icon-back" ] [] ]
            , ul
                [ class "links-head" ]
                [ li participantsLinkAttributes
                    [ a [] [ span [ class "icon-mother" ] [] ] ]
                , li activitiesLinkAttributes
                    [ a [] [ span [ class "icon-activity" ] [] ] ]
                ]
            ]
