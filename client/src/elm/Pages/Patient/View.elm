module Pages.Patient.View
    exposing
        ( viewChild
        , viewMother
        )

import App.PageType
import Child.Model exposing (Child, ChildId)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mother.Model exposing (Mother, MotherId)
import Pages.Patient.Model exposing (Msg(..))
import Patient.Model exposing (PatientId, Patient, PatientType(..))
import RemoteData exposing (RemoteData(..), WebData)
import User.Model exposing (User)


viewChild : Date -> User -> ChildId -> Child -> WebData Patient -> Html Msg
viewChild currentDate currentUser childId child motherWebData =
    let
        motherInfo =
            case motherWebData of
                Success patient ->
                    case patient.info of
                        PatientMother mother ->
                            div []
                                [ text <| "Mother: "
                                , a
                                    [ href "#"
                                    , onClick <| SetRedirectPage (App.PageType.Patient child.motherId)
                                    ]
                                    [ text mother.name ]
                                ]

                        _ ->
                            div [] []

                _ ->
                    div [] [ i [ class "icon loading spinner" ] [] ]
    in
        div []
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text child.name ]
                ]
            , div []
                [ img [ src child.image ] []
                ]
            , div
                [ class "ui divider" ]
                [ motherInfo ]
            ]


viewMother : Date -> User -> MotherId -> Mother -> Html Msg
viewMother currentDate currentUser motherId mother =
    div []
        [ div
            [ class "ui secondary pointing fluid menu" ]
            [ h1
                [ class "ui header" ]
                [ text mother.name ]
            ]
        , div []
            [ img [ src mother.image ] []
            ]
        , div
            [ class "ui divider" ]
            []
        ]
