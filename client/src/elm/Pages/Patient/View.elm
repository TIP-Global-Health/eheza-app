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
import RemoteData exposing (RemoteData(..), WebData)
import User.Model exposing (User)


viewChild : Date -> User -> ChildId -> Child -> WebData Mother -> Html Msg
viewChild currentDate currentUser childId child motherWebData =
    let
        motherInfo =
            case motherWebData of
                Success mother ->
                    div []
                        [ text <| "Mother: "
                        , a
                            [ href "#"
                            , onClick <| SetRedirectPage (App.PageType.Patient child.motherId)
                            ]
                            [ text mother.name ]
                        ]

                Loading ->
                    div []
                        [ text <| "Mother: "
                        , i [ class "icon loading spinner" ] []
                        ]

                _ ->
                    div [] []
    in
        div []
            [ div
                [ class "ui secondary pointing fluid menu" ]
                [ h1
                    [ class "ui header" ]
                    [ text child.name ]
                ]
            , div [ class "ui card" ]
                [ img [ src child.image ] []
                , div [ class "content" ] [ motherInfo ]
                ]
            , div
                [ class "ui divider" ]
                []
            ]


viewMother : Date -> User -> MotherId -> Mother -> List (WebData ( ChildId, Child )) -> Html Msg
viewMother currentDate currentUser motherId mother children =
    let
        childrenInfo =
            (List.map
                (\childWebData ->
                    case childWebData of
                        Success ( childId, child ) ->
                            li []
                                [ a
                                    [ href "#"
                                    , onClick <| SetRedirectPage (App.PageType.Patient childId)
                                    ]
                                    [ text child.name ]
                                ]

                        Loading ->
                            li []
                                [ i [ class "icon loading spinner" ] []
                                ]

                        _ ->
                            div [] []
                )
                children
            )
    in
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
                [ text <| "Children: "
                , ul [] childrenInfo
                ]
            ]
