module Pages.Attendance.View exposing (view)

{-| This shows the page where we can record which mothers are in attendance
at a session.

<https://github.com/Gizra/ihangane/issues/409>

-}

import Activity.Utils exposing (isCheckedIn)
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import Backend.Mother.Model exposing (Mother)
import EveryDictList
import Html exposing (..)
import Html.Attributes exposing (..)
import Translate exposing (translate, Language)


view : Language -> EditableSession -> Html a
view language session =
    let
        mothers =
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
                , a [ class "link-back" ]
                    [ span [ class "icon-back" ] []
                    , span [] []
                    ]

                -- TODO: This should be a function, since we can use it on all the session pages
                , ul [ class "links-head" ]
                    [ li
                        [ class "active" ]
                        [ a [] [ span [ class "icon-completed" ] [] ] ]
                    , li
                        []
                        [ a [] [ span [ class "icon-mother" ] [] ] ]
                    , li
                        []
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


viewMother : EditableSession -> MotherId -> Mother -> Html msg
viewMother session motherId mother =
    let
        checkIn =
            if isCheckedIn motherId session then
                a
                    [ class "link-checked-in" ]
                    [ span [ class "icon-checked-in" ] [] ]
            else
                a
                    [ class "link-check-in" ]
                    [ span [ class "icon-check-in" ] [] ]
    in
        div
            [ class "item" ]
            [ checkIn
            , img
                [ alt mother.name
                , class "ui image"
                , height 110
                , width 110
                , src "assets/images/profile-baby.jpg"
                ]
                []
            , div
                [ class "content" ]
                [ text mother.name ]
            ]
