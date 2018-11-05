module Pages.Attendance.View exposing (view)

{-| This shows the page where we can record which mothers are in attendance
at a session.

<https://github.com/Gizra/ihangane/issues/409>

-}

import Activity.Utils exposing (motherIsCheckedIn)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (EditableSession, MsgEditableSession(..))
import EveryDictList
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Model exposing (MsgSession(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)


view : Language -> EditableSession -> Html MsgSession
view language session =
    let
        mothers =
            if EveryDictList.isEmpty session.offlineSession.mothers then
                [ div
                    [ class "ui message warning" ]
                    [ text <| translate language Translate.ThisClinicHasNoMothers ]
                ]

            else
                session.offlineSession.mothers
                    |> EveryDictList.map (viewMother session)
                    |> EveryDictList.values
    in
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.Attendance ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            , ul [ class "links-head" ]
                [ li
                    [ class "active" ]
                    [ a [] [ span [ class "icon-completed" ] [] ] ]
                , li
                    [ onClick <| SetActivePage <| SessionPage ParticipantsPage ]
                    [ a [] [ span [ class "icon-mother" ] [] ] ]
                , li
                    [ onClick <| SetActivePage <| SessionPage ActivitiesPage ]
                    [ a [] [ span [ class "icon-measurements" ] [] ] ]
                ]
            ]
        , div
            [ class "ui full blue segment" ]
            [ div
                [ class "full content" ]
                [ div [ class "wrap-list" ]
                    [ h3
                        [ class "ui header" ]
                        [ text <| translate language Translate.CheckIn ]
                    , p [] [ text <| translate language Translate.ClickTheCheckMark ]
                    , div [ class "ui middle aligned divided list" ] mothers
                    ]
                ]
            ]
        ]


viewMother : EditableSession -> MotherId -> Mother -> Html MsgSession
viewMother session motherId mother =
    let
        checkIn =
            if motherIsCheckedIn motherId session then
                a
                    [ class "link-checked-in"
                    , onClick <| MsgEditableSession <| SetCheckedIn motherId False
                    ]
                    [ span [ class "icon-checked-in" ] [] ]

            else
                a
                    [ class "link-check-in"
                    , onClick <| MsgEditableSession <| SetCheckedIn motherId True
                    ]
                    [ span [ class "icon-check-in" ] [] ]
    in
    div
        [ class "item" ]
        [ checkIn
        , thumbnailImage "mother" mother.avatarUrl mother.name 110 110
        , div
            [ class "content" ]
            [ text mother.name ]
        ]
