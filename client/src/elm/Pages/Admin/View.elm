module Pages.Admin.View exposing (view)

import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelBackend)
import Backend.Session.Model exposing (Session)
import Config.Model as Config
import EveryDictList exposing (EveryDictList)
import EverySet
import Gizra.Html exposing (showIf)
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


view : Config.Model -> Language -> NominalDate -> User -> ModelBackend -> Html Msg
view config language currentDate user backend =
    let
        content =
            if EverySet.member Administrator user.roles then
                contentForAdmin config language currentDate backend
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


contentForAdmin : Config.Model -> Language -> NominalDate -> ModelBackend -> Html Msg
contentForAdmin config language currentDate backend =
    div [] <|
        viewOrFetch language
            (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
            (viewLoadedClinics config language currentDate backend)
            identity
            backend.clinics


viewLoadedClinics : Config.Model -> Language -> NominalDate -> ModelBackend -> EveryDictList ClinicId Clinic -> List (Html Msg)
viewLoadedClinics config language currentDate backend clinics =
    viewOrFetch language
        (MsgLoggedIn <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate)
        (viewLoadedSessions config language clinics)
        identity
        backend.futureSessions


viewLoadedSessions : Config.Model -> Language -> EveryDictList ClinicId Clinic -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewLoadedSessions config language clinics ( _, futureSessions ) =
    let
        futureSessionsByClinic =
            EveryDictList.groupBy (Tuple.second >> .clinicId) (EveryDictList.toList futureSessions)
    in
        clinics
            |> EveryDictList.sortBy .name
            |> EveryDictList.toList
            |> List.map (viewClinic language futureSessionsByClinic)


viewClinic : Language -> EveryDictList ClinicId (List ( SessionId, Session )) -> ( ClinicId, Clinic ) -> Html Msg
viewClinic language futureSessionsByClinic ( clinicId, clinic ) =
    let
        futureSessions =
            futureSessionsByClinic
                |> EveryDictList.get clinicId
                |> Maybe.withDefault []
                |> List.sortBy (Tuple.second >> .scheduledDate >> .start >> Time.Date.toTuple)
    in
        div []
            [ h2 [] [ text clinic.name ]
            , viewFutureSessions language futureSessions
            ]


viewFutureSessions : Language -> List ( SessionId, Session ) -> Html Msg
viewFutureSessions language sessions =
    table
        [ class "ui striped table" ]
        [ thead [] []
        , tbody [] (List.map (viewFutureSession language) sessions)
        , tfoot [] []
        ]


viewFutureSession : Language -> ( SessionId, Session ) -> Html Msg
viewFutureSession language ( sessionId, session ) =
    tr []
        [ td []
            [ text <| formatYYYYMMDD session.scheduledDate.start
            , text " - "
            , text <| formatYYYYMMDD session.scheduledDate.end
            , text " "
            , showIf session.training <|
                span
                    [ class "ui teal tag label training-label" ]
                    [ text <| translate language Translate.Training ]
            ]
        ]
