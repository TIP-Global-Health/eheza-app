module Pages.Clinics.View exposing (view)

{-| The purpose of this page is to show a list of clinics, allowing the
user to click on clinics the user is assigned to, to see the sessions which are
available for data-entry.
-}

import Html exposing (..)
import App.Model exposing (Msg(..), MsgLoggedIn(..))
import Backend.Clinic.Model exposing (Clinic)
import Backend.Session.Model exposing (Session, EditableSession)
import Backend.Entities exposing (ClinicId, SessionId)
import Backend.Model exposing (MsgBackend(..))
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..), SessionPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date exposing (delta)
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import User.Utils exposing (assignedToClinic)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewOrFetch)


{-| To make things simpler, we just supply the whole state of the backend ... the view
can pick out what it wants. (Sometimes it would be a better idea to pass more
specific things, rather than the whole state of the backend).

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

If `selectedClinic` is Just, we'll show a page for that clinic. If not, we'll
show a list of clinics.

The `cachedSession` param represents the `OfflineSession` from our cache.
Loading the offline session from the cache is essential for our startup logic ..
since if we have a session with edits, we'll bypass all of this and show the
editing UI for the session. So, we demand here that the caller definitely knows
whether the offlineSession exists or not ... if it's still a `NotAsked`, then
you just shouldn't call us yet ... you should show something else while you
figure that out. In other words, a `Nothing` for `cachedSession` means we
definitely don't have one, not that we're still checking. Note that we don't
actually need the `OfflineSession` itself here, but if you provide the `SessionId`,
you are asserting that you'll also be able to provide the `OfflineSession` itself
when needed.

Also, you shouldn't call us if you already have edits for the offline session
... in that case, you should show the editing UI and insist that the edits be
uploaded before showing this again.

-}
view : Language -> NominalDate -> User -> Maybe ClinicId -> Backend.Model.ModelBackend -> Maybe ( SessionId, EditableSession ) -> Html Msg
view language currentDate user selectedClinic backend cachedSession =
    case selectedClinic of
        Just clinicId ->
            viewClinic language currentDate clinicId backend cachedSession

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
session at a time at the moment (we could change that in future).

We consult the `futureSessions` in the backend to implement the "you can only
download one day before" rule mentioned in the explanatory text on the screen.

TODO: We don't actually implement the "one day before" rule on the backend yet,
just in the UI.

TODO: We don't check whether the user is authorized to view this clinic here
... the `viewClinicList` function won't enable a link if the user isn't
authorized, but we should check here as well, in case a crafty user just types
in a URL to get here.

-}
viewClinic : Language -> NominalDate -> ClinicId -> Backend.Model.ModelBackend -> Maybe ( SessionId, EditableSession ) -> Html Msg
viewClinic language currentDate clinicId backend cachedSession =
    case cachedSession of
        Just ( sessionId, session ) ->
            -- If we do have a cached session, then show something that depends on its status
            viewClinicWithCachedSession language clinicId backend.offlineSessionRequest sessionId session

        Nothing ->
            -- If we don't have a cached session, show the UI for getting one
            div [ class "wrap wrap-alt-2" ] <|
                viewOrFetch language
                    (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
                    (\clinics ->
                        viewOrFetch language
                            (MsgLoggedIn <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate)
                            (\sessions -> viewLoadedClinic language currentDate clinicId clinics backend.offlineSessionRequest sessions)
                            backend.futureSessions
                    )
                    backend.clinics


viewLoadedClinic : Language -> NominalDate -> ClinicId -> EveryDictList ClinicId Clinic -> WebData SessionId -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewLoadedClinic language currentDate clinicId clinics request ( queryDate, futureSessions ) =
    case EveryDictList.get clinicId clinics of
        Just clinic ->
            viewFoundClinic language currentDate clinicId clinic request futureSessions

        Nothing ->
            [ Pages.PageNotFound.View.viewPage language <|
                UserPage <|
                    ClinicsPage <|
                        Just clinicId
            ]


viewFoundClinic : Language -> NominalDate -> ClinicId -> Clinic -> WebData SessionId -> EveryDictList SessionId Session -> List (Html Msg)
viewFoundClinic language currentDate clinicId clinic request sessions =
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
                            && (Time.Date.compare session.scheduledDate.end currentDate /= LT)
                    )
                |> EveryDictList.head
                |> Maybe.map Tuple.first

        downloadAttrs =
            case validSession of
                Just sessionId ->
                    [ class "ui fluid primary button"
                    , onClick <| MsgLoggedIn <| MsgBackend <| FetchOfflineSessionFromBackend sessionId
                    ]

                Nothing ->
                    [ class "ui fluid primary dark button disabled" ]

        content =
            [ div
                [ class "ui info" ]
                [ p [] [ text <| translate language Translate.DownloadSession1 ]
                , p [] [ text <| translate language Translate.DownloadSession2 ]
                ]
            , button downloadAttrs
                [ text <| translate language <| Translate.DownloadHealthAssessment ]
            ]
    in
        [ showMaybe <| Maybe.map (viewDownloadProgress language request) validSession
        , div
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
            content
        ]


viewDownloadProgress : Language -> WebData SessionId -> SessionId -> Html Msg
viewDownloadProgress language request validSession =
    viewModal <|
        case request of
            NotAsked ->
                Nothing

            Loading ->
                Just <|
                    div
                        [ class "ui tiny inverted active modal" ]
                        [ div
                            [ class "header" ]
                            [ text <| translate language Translate.DownloadingSession1 ]
                        , div
                            [ class "content" ]
                            [ div [ class "ui active centered massive inline loader" ] []
                            , p [] [ text <| translate language Translate.DownloadingSession2 ]
                            ]
                        ]

            Failure err ->
                -- TODO: We could do something with the err ...
                Just <|
                    div
                        [ class "ui tiny inverted active modal" ]
                        [ div
                            [ class "header" ]
                            [ text <| translate language Translate.UnableToDownload ]
                        , div
                            [ class "content" ]
                            [ p [] [ text <| translate language Translate.MakeSureYouAreConnected ]
                            ]
                        , div
                            [ class "actions" ]
                            [ div
                                [ class "two basic ui buttons" ]
                                [ button
                                    [ class "ui fluid button"
                                    , onClick <| MsgLoggedIn <| MsgBackend <| ResetOfflineSessionRequest
                                    ]
                                    [ text <| translate language Translate.OK ]
                                ]
                            ]
                        ]

            Success sessionId ->
                if sessionId == validSession then
                    Just <|
                        div
                            [ class "ui tiny inverted active modal" ]
                            [ div
                                [ class "header" ]
                                [ text <| translate language Translate.DownloadSuccessful ]
                            , div
                                [ class "content" ]
                                [ span [ class "icon-success" ] []
                                , p [] [ text <| translate language Translate.ReadyToBeginSession ]
                                ]
                            , div
                                [ class "actions" ]
                                [ div
                                    [ class "two basic ui buttons" ]
                                    [ button
                                        [ class "ui fluid button"
                                        , onClick <| MsgLoggedIn <| MsgBackend <| ResetOfflineSessionRequest
                                        ]
                                        [ text <| translate language Translate.OK ]
                                    ]
                                ]
                            ]
                else
                    Nothing


viewClinicWithCachedSession : Language -> ClinicId -> WebData SessionId -> SessionId -> EditableSession -> Html Msg
viewClinicWithCachedSession language clinicId downloadRequest sessionId session =
    let
        content =
            if session.offlineSession.session.clinicId == clinicId then
                if session.edits.explicitlyClosed then
                    [ div
                        [ class "ui info" ]
                        [ p [] [ text <| translate language Translate.YouHaveACompletedSession ]
                        ]
                    , button
                        [ class "ui fluid primary button" ]
                        [ text <| translate language Translate.UploadHealthAssessment ]
                    ]
                else
                    [ button
                        [ class "ui fluid primary button"
                        , onClick <| SetActivePage (SessionPage AttendancePage)
                        ]
                        [ text <| translate language Translate.BeginHealthAssessment ]
                    ]
            else
                [ p [] [ text <| translate language Translate.SessionInProgress ]
                , button
                    [ class "ui fluid primary dark button"
                    , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just clinicId
                    ]
                    [ text activeClinicName ]
                ]

        activeClinicName =
            EveryDictList.get session.offlineSession.session.clinicId session.offlineSession.clinics
                |> Maybe.map .name
                |> Maybe.withDefault (translate language Translate.ClinicNotFound)

        clinicName =
            EveryDictList.get clinicId session.offlineSession.clinics
                |> Maybe.map .name
                |> Maybe.withDefault (translate language Translate.ClinicNotFound)
    in
        div [ class "wrap wrap-alt-2" ]
            [ viewDownloadProgress language downloadRequest sessionId
            , div
                [ class "ui basic head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text clinicName ]
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
                content
            ]
