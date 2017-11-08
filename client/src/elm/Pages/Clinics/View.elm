module Pages.Clinics.View exposing (view)

{-| The purpose of this page is to show a list of clinics, allowing the
user to click on clinics the user is assigned to, to see the sessions which are
available for data-entry.
-}

import Html exposing (..)
import App.Model exposing (Msg(..), MsgLoggedIn(..))
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (ClinicId, SessionId)
import Backend.Model
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import User.Utils exposing (assignedToClinic)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| In principle, one could just supply the LoggedInModel, but it's nice to avoid
that, where possible ... we only need the Clinics and the user.

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

-}
view : Language -> User -> WebData (EveryDictList ClinicId Clinic) -> Html Msg
view language user clinicData =
    div [ class "wrap wrap-alt-2" ]
        [ div [ class "ui basic head segment" ]
            [ h1 [ class "ui header" ]
                [ text <| translate language Translate.Clinics ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage LoginPage
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui basic segment" ]
            (viewWrapper language user clinicData)
        ]


viewWrapper : Language -> User -> WebData (EveryDictList ClinicId Clinic) -> List (Html Msg)
viewWrapper language user clinicData =
    -- Note that we should get through to the `viewWithData` case, since we'll
    -- automatically trigger the necessary fetches. Of course, I suppose an error
    -- is always possible ...
    --
    -- Clearly, the pattern below is some kind of variant on `RemoteData.map` ...
    -- perhaps there is a `RemoteData.view` function lurking here.
    case clinicData of
        NotAsked ->
            -- since it will be automatic
            [ spinner ]

        Loading ->
            [ spinner ]

        Failure err ->
            [ viewError language err
            , div
                [ class "ui button"
                , onClick (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
                ]
                [ text <| translate language Translate.Retry ]
            ]

        Success clinics ->
            viewWithData language user clinics


{-| This is the "inner" view function ... we get here if all the data was actually available.
-}
viewWithData : Language -> User -> EveryDictList ClinicId Clinic -> List (Html Msg)
viewWithData language user clinics =
    let
        title =
            p
                [ class "centered" ]
                [ text <| translate language Translate.SelectYourClinic
                , text ":"
                ]

        clinicView =
            clinics
                |> EveryDictList.toList
                |> List.map (viewClinic user)
    in
        title :: clinicView


viewClinic : User -> ( ClinicId, Clinic ) -> Html Msg
viewClinic user ( clinicId, clinic ) =
    let
        classAttr =
            if assignedToClinic clinicId user then
                class "ui fluid primary button"
            else
                class "ui fluid primary dark disabled button"
    in
        button
            [ classAttr ]
            [ text clinic.name ]
