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
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import User.Utils exposing (assignedToClinic)
import Utils.WebData exposing (viewOrFetch)


{-| To make things simpler, we just supply the whole state of the backend ... the view
can pick out what it wants. (Sometimes it would be a better idea to pass more
specific things, rather than the whole state of the backend).

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

If `selectedClinic` is Just, we'll show a page for that clinic. If not, we'll
show a list of clinics.

-}
view : Language -> User -> Maybe ClinicId -> Backend.Model.Model -> Html Msg
view language user selectedClinic backend =
    case selectedClinic of
        Just clinicId ->
            viewClinic language clinicId backend

        Nothing ->
            viewClinicList language user backend.clinics


viewClinicList : Language -> User -> WebData (EveryDictList ClinicId Clinic) -> Html Msg
viewClinicList language user clinicData =
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
        , clinicData
            |> viewOrFetch language
                (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
                (viewLoadedClinicList language user)
            |> div [ class "ui basic segment" ]
        ]


{-| This is the "inner" view function ... we get here if all the data was actually available.
-}
viewLoadedClinicList : Language -> User -> EveryDictList ClinicId Clinic -> List (Html Msg)
viewLoadedClinicList language user clinics =
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
                |> List.map (viewClinicButton user)
    in
        title :: clinicView


viewClinicButton : User -> ( ClinicId, Clinic ) -> Html Msg
viewClinicButton user ( clinicId, clinic ) =
    let
        classAttr =
            if assignedToClinic clinicId user then
                class "ui fluid primary button"
            else
                class "ui fluid primary dark disabled button"
    in
        button
            [ classAttr
            , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just clinicId
            ]
            [ text clinic.name ]


{-| View a specific clinic ...

<https://github.com/Gizra/ihangane/issues/407>

-}
viewClinic : Language -> ClinicId -> Backend.Model.Model -> Html Msg
viewClinic language clinic backend =
    div [] [ text "clinic goes here" ]
