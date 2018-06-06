module Pages.Admin.View exposing (view)

import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelBackend)
import Backend.Session.Model exposing (Session)
import EveryDictList exposing (EveryDictList)
import EverySet
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import App.Model exposing (Msg(..), MsgLoggedIn(..))
import Pages.Page exposing (..)
import Translate exposing (Language, translate)
import Time.Date
import User.Model exposing (..)
import Utils.WebData exposing (viewOrFetch)


view : Language -> NominalDate -> User -> ModelBackend -> Html Msg
view language currentDate user backend =
    let
        content =
            if EverySet.member Administrator user.roles then
                contentForAdmin language currentDate backend
            else
                contentForOthers language
    in
        div [ class "wrap wrap-alt-2 admin-page" ]
            [ div
                [ class "ui basic head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Translate.Admin ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage LoginPage
                    ]
                    [ span [ class "icon-back" ] []
                    , span [] []
                    ]
                ]
            , div [ class "ui basic segment" ] [ content ]
            ]


contentForOthers : Language -> Html Msg
contentForOthers language =
    div [ class "ui basic segment" ]
        [ text <| translate language Translate.YouAreNotAnAdmin ]


contentForAdmin : Language -> NominalDate -> ModelBackend -> Html Msg
contentForAdmin language currentDate backend =
    div [] <|
        viewOrFetch language
            (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
            (viewLoadedClinics language currentDate backend)
            identity
            backend.clinics


viewLoadedClinics : Language -> NominalDate -> ModelBackend -> EveryDictList ClinicId Clinic -> List (Html Msg)
viewLoadedClinics language currentDate backend clinics =
    viewOrFetch language
        (MsgLoggedIn <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate)
        (viewLoadedSessions language clinics)
        identity
        backend.futureSessions


viewLoadedSessions : Language -> EveryDictList ClinicId Clinic -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewLoadedSessions language clinics ( _, futureSessions ) =
    let
        futureSessionsByClinic =
            EveryDictList.groupBy (Tuple.second >> .clinicId) (EveryDictList.toList futureSessions)
    in
        [ table
            [ class "ui striped table" ]
            [ thead []
                [ tr []
                    [ th [] [ text <| translate language Translate.Clinics ]
                    , th [] [ text <| translate language Translate.FutureSessions ]
                    ]
                ]
            , tbody []
                (List.map (viewClinicRow futureSessionsByClinic) <| EveryDictList.toList clinics)
            , tfoot [] []
            ]
        ]


viewClinicRow : EveryDictList ClinicId (List ( SessionId, Session )) -> ( ClinicId, Clinic ) -> Html Msg
viewClinicRow futureSessionsByClinic ( clinicId, clinic ) =
    let
        futureSessions =
            futureSessionsByClinic
                |> EveryDictList.get clinicId
                |> Maybe.withDefault []
                |> List.sortBy (Tuple.second >> .scheduledDate >> .start >> Time.Date.toTuple)
    in
        tr []
            [ td [] [ text clinic.name ]
            , td [] (viewFutureSessions futureSessions)
            ]


viewFutureSessions : List ( SessionId, Session ) -> List (Html Msg)
viewFutureSessions sessions =
    [ table
        [ class "ui table" ]
        [ thead [] []
        , tbody [] (List.map viewFutureSession sessions)
        , tfoot [] []
        ]
    ]


viewFutureSession : ( SessionId, Session ) -> Html Msg
viewFutureSession ( sessionId, session ) =
    tr []
        [ td []
            [ text <| formatYYYYMMDD session.scheduledDate.start
            , text " - "
            , text <| formatYYYYMMDD session.scheduledDate.end
            ]
        ]
