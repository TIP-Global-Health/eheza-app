module Pages.Attendance.View exposing (view)

{-| This shows the page where we can record which mothers are in attendance
at a session.

<https://github.com/Gizra/ihangane/issues/409>

-}

import Activity.Utils exposing (isCheckedIn)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (EditableSession)
import EveryDictList
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.Attendance.Model exposing (..)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)


view : Language -> EditableSession -> Model -> Html Msg
view language session model =
    let
        filter =
            String.trim model.filter

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
                , Just session.offlineSession.session.clinicId
                    |> ClinicsPage
                    |> UserPage
                    |> SetActivePage
                    |> onClick
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
                    , div
                        [ class "ui action input small" ]
                        [ input
                            [ placeholder <| translate language Translate.FilterByName
                            , type_ "text"
                            , onInput SetFilter
                            , value model.filter
                            ]
                            []
                        , button
                            [ classList
                                [ ( "ui button", True )
                                , ( "disabled", String.isEmpty filter )
                                ]
                            , onClick <| SetFilter ""
                            ]
                            [ text <| translate language Translate.ShowAll ]
                        ]
                    , div [ class "ui middle aligned divided list" ] mothers
                    ]
                ]
            ]
        ]


viewMother : EditableSession -> MotherId -> Mother -> Html Msg
viewMother session motherId mother =
    let
        checkIn =
            if isCheckedIn motherId session then
                a
                    [ class "link-checked-in"
                    , onClick <| SetCheckedIn motherId False
                    ]
                    [ span [ class "icon-checked-in" ] [] ]

            else
                a
                    [ class "link-check-in"
                    , onClick <| SetCheckedIn motherId True
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
