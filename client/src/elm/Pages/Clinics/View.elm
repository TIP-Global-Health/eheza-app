module Pages.Clinics.View exposing (view)

{-| The purpose of this page is to show a list of clinics, allowing the
user to click on clinics the user is assigned to, to see the sessions which are
available for data-entry.
-}

import Html exposing (..)
import App.Model exposing (Msg(..), MsgLoggedIn(..))
import Backend.Clinic.Model exposing (Clinic)
import Backend.Session.Model exposing (Session)
import Backend.Entities exposing (ClinicId, SessionId)
import Backend.Model
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date exposing (delta)
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

You shouldn't call this if you already have an OfflineSession to edit ... in that
case, you ought to show the UI for the OfflineSession instead. (In future, we could
change that and allow viewing this even if you have an offline session, but we'd need
to define what that would look like).

-}
view : Language -> NominalDate -> User -> Maybe ClinicId -> Backend.Model.ModelBackend -> Html Msg
view language currentDate user selectedClinic backend =
    case selectedClinic of
        Just clinicId ->
            viewClinic language currentDate clinicId backend

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

Basically, this view is meant to show the clinic, and offer an opportunity to
download a session for offline editing. We are only supporting one offline
session at a time at the moment (we could change that in future). We don't
enforce that here ... if we already have an offline session, it's the caller's
responsibility not to call us.

We consult the `futureSessions` in the backend to implement the "you can only
download one day before" rule mentioned in the explanatory text on the screen.

TODO: We don't actually implement the "one day before" rule on the backend yet,
just in the UI.

TODO: We don't check whether the user is authorized to view this clinic here
... the `viewClinicList` function won't enable a link if the user isn't
authorized, but we should check here as well, in case a crafty user just types
in a URL to get here.

-}
viewClinic : Language -> NominalDate -> ClinicId -> Backend.Model.ModelBackend -> Html Msg
viewClinic language currentDate clinicId backend =
    div [ class "wrap wrap-alt-2" ] <|
        viewOrFetch language
            (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
            (\clinics ->
                viewOrFetch language
                    (MsgLoggedIn <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate)
                    (\sessions -> viewLoadedClinic language currentDate clinicId clinics sessions)
                    backend.futureSessions
            )
            backend.clinics


viewLoadedClinic : Language -> NominalDate -> ClinicId -> EveryDictList ClinicId Clinic -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewLoadedClinic language currentDate clinicId clinics ( queryDate, futureSessions ) =
    case EveryDictList.get clinicId clinics of
        Just clinic ->
            viewFoundClinic language currentDate clinicId clinic futureSessions

        Nothing ->
            [ Pages.PageNotFound.View.viewPage language <|
                UserPage <|
                    ClinicsPage <|
                        Just clinicId
            ]


viewFoundClinic : Language -> NominalDate -> ClinicId -> Clinic -> EveryDictList SessionId Session -> List (Html Msg)
viewFoundClinic language currentDate clinicId clinic sessions =
    let
        validSession =
            sessions
                |> EveryDictList.filter
                    (\_ session ->
                        -- Must be the appropriate clinicId
                        (session.clinicId == clinicId)
                            -- And start date must be in past, or at most one day in future
                            && ((delta session.scheduledDate.start currentDate).days <= 1)
                            -- And end date must be in the future
                            && (Time.Date.compare session.scheduledDate.end currentDate /= GT)
                    )
                |> EveryDictList.head

        downloadAttrs =
            case validSession of
                Just ( sessionId, session ) ->
                    [ class "ui fluid primary button"

                    -- TODO: onClick
                    ]

                Nothing ->
                    [ class "ui fluid primary dark button disabled" ]
    in
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text clinic.name ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage Nothing
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui basic wide segment" ]
            [ div
                [ class "ui info" ]
                [ p [] [ text <| translate language Translate.DownloadSession1 ]
                , p [] [ text <| translate language Translate.DownloadSession2 ]
                ]
            , button downloadAttrs
                [ text <| translate language <| Translate.DownloadHealthAssessment ]
            ]
        ]
