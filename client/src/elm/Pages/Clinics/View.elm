module Pages.Clinics.View exposing (view)

{-| The purpose of this page is to show a list of clinics, allowing the
user to click on clinics the user is assigned to, to see the sessions which are
available for data-entry.
-}

import App.Model exposing (Msg(..), MsgLoggedIn(..))
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (ClinicId, SessionId)
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date exposing (delta)
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import User.Utils exposing (assignedToClinic)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewError, viewOrFetch)


{-| To make things simpler, we just supply the whole state of the backend ... the view
can pick out what it wants. (Sometimes it would be a better idea to pass more
specific things, rather than the whole state of the backend).

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

If `selectedClinic` is Just, we'll show a page for that clinic. If not, we'll
show a list of clinics.

-}
view : Language -> NominalDate -> User -> Maybe ClinicId -> ModelBackend -> ModelCached -> Html Msg
view language currentDate user selectedClinic backend cache =
    case selectedClinic of
        Just clinicId ->
            viewClinic language currentDate clinicId backend cache

        Nothing ->
            viewClinicList language user backend.clinics cache


viewClinicList : Language -> User -> WebData (EveryDictList ClinicId Clinic) -> ModelCached -> Html Msg
viewClinicList language user clinicData cache =
    let
        session =
            cache.editableSession
                |> RemoteData.toMaybe
                |> Maybe.Extra.join
                |> Maybe.map Tuple.second

        content =
            -- We get the clinics from the session, if one is loaded, or we rely on
            -- being online, if not.
            case session of
                Just loaded ->
                    viewLoadedClinicList language user loaded.offlineSession.clinics

                Nothing ->
                    viewOrFetch language
                        (MsgLoggedIn <| MsgBackend Backend.Model.FetchClinics)
                        (viewLoadedClinicList language user)
                        identity
                        clinicData
    in
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
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
        , div [ class "ui basic segment" ] content
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
viewClinic : Language -> NominalDate -> ClinicId -> ModelBackend -> ModelCached -> Html Msg
viewClinic language currentDate clinicId backend cache =
    let
        cachedSession =
            cache.editableSession
                |> RemoteData.toMaybe
                |> Maybe.Extra.join
    in
    case cachedSession of
        Just ( sessionId, session ) ->
            -- If we do have a cached session, then show something that depends on its status
            viewClinicWithCachedSession language clinicId backend cache sessionId session

        Nothing ->
            -- If we don't have a cached session, show the UI for getting one/
            -- TODO: Make this less awkward
            let
                wrapError html =
                    [ div
                        [ class "ui basic head segment" ]
                        [ h1
                            [ class "ui header" ]
                            [ text <| translate language Translate.ClinicNotFound ]
                        , a
                            [ class "link-back"
                            , onClick <| SetActivePage <| UserPage <| ClinicsPage Nothing
                            ]
                            [ span [ class "icon-back" ] []
                            , span [] []
                            ]
                        ]
                    , div [ class "ui basic wide segment" ] html
                    ]

                content =
                    viewOrFetch language
                        (MsgLoggedIn <| MsgBackend <| Backend.Model.FetchClinics)
                        (\clinics ->
                            viewOrFetch language
                                (MsgLoggedIn <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate)
                                (\sessions -> viewLoadedClinic language currentDate clinicId clinics backend cache sessions)
                                wrapError
                                backend.futureSessions
                        )
                        wrapError
                        backend.clinics
            in
            div [ class "wrap wrap-alt-2" ] content


viewLoadedClinic : Language -> NominalDate -> ClinicId -> EveryDictList ClinicId Clinic -> ModelBackend -> ModelCached -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewLoadedClinic language currentDate clinicId clinics backend cache ( queryDate, futureSessions ) =
    case EveryDictList.get clinicId clinics of
        Just clinic ->
            viewFoundClinic language currentDate clinicId clinic backend cache futureSessions

        Nothing ->
            [ Pages.PageNotFound.View.viewPage language (SetActivePage LoginPage) <|
                UserPage <|
                    ClinicsPage <|
                        Just clinicId
            ]


viewFoundClinic : Language -> NominalDate -> ClinicId -> Clinic -> ModelBackend -> ModelCached -> EveryDictList SessionId Session -> List (Html Msg)
viewFoundClinic language currentDate clinicId clinic backend cache sessions =
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
    [ showMaybe <| Maybe.map (viewDownloadProgress language backend.offlineSessionRequest cache.cacheStorage.cachedPhotos) validSession
    , viewUploadProgress language backend.uploadEditsRequest
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


{-| The `WebData SessionId` represents a request to download the offline session.

The `WebData (List String)` represents a request to cache the photos that the
offline session needs.

-}
viewDownloadProgress : Language -> WebData SessionId -> WebData (List String) -> SessionId -> Html Msg
viewDownloadProgress language request photos validSession =
    viewModal <|
        case RemoteData.append request photos of
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
                let
                    log =
                        Debug.log "err" err
                in
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

            Success ( sessionId, photoUrls ) ->
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


viewUploadProgress : Language -> WebData SessionId -> Html Msg
viewUploadProgress language request =
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
                            [ text <| translate language Translate.UploadingSession1 ]
                        , div
                            [ class "content" ]
                            [ div [ class "ui active centered massive inline loader" ] []
                            , p [] [ text <| translate language Translate.UploadingSession2 ]
                            ]
                        ]

            Failure err ->
                -- TODO: We could do something with the err ...
                Just <|
                    div
                        [ class "ui tiny inverted active modal" ]
                        [ div
                            [ class "header" ]
                            [ text <| translate language Translate.UnableToUpload ]
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
                                    , onClick <| MsgLoggedIn <| MsgBackend <| ResetUploadEditsRequest
                                    ]
                                    [ text <| translate language Translate.OK ]
                                ]
                            ]
                        ]

            Success sessionId ->
                Just <|
                    div
                        [ class "ui tiny inverted active modal" ]
                        [ div
                            [ class "header" ]
                            [ text <| translate language Translate.UploadSuccessful ]
                        , div
                            [ class "content" ]
                            [ span [ class "icon-success" ] []
                            , p [] [ text <| translate language Translate.DataIsNowSaved ]
                            ]
                        , div
                            [ class "actions" ]
                            [ div
                                [ class "two basic ui buttons" ]
                                [ button
                                    [ class "ui fluid button"
                                    , onClick <| MsgLoggedIn <| MsgBackend <| ResetUploadEditsRequest
                                    ]
                                    [ text <| translate language Translate.OK ]
                                ]
                            ]
                        ]


{-| This is where we get if we're trying to view a clinic and we've got a session cached locally.
-}
viewClinicWithCachedSession : Language -> ClinicId -> ModelBackend -> ModelCached -> SessionId -> EditableSession -> Html Msg
viewClinicWithCachedSession language clinicId backend cache sessionId session =
    let
        content =
            if session.offlineSession.session.clinicId == clinicId then
                if session.edits.explicitlyClosed then
                    [ div
                        [ class "ui info" ]
                        [ p [] [ text <| translate language Translate.YouHaveACompletedSession ]
                        ]

                    -- TODO: Need to consider whether we're logged in or not
                    -- ... so, actually need to be able to show this when we're
                    -- **not** logged in. So, we'll need a `Maybe User` above
                    -- (or something like that).
                    , button
                        [ class "ui fluid primary button"
                        , onClick <| MsgLoggedIn <| MsgBackend <| UploadEdits sessionId session.edits
                        ]
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
                [ div
                    [ class "ui info" ]
                    [ p [] [ text <| translate language Translate.SessionInProgress ] ]
                , button
                    [ class "ui fluid primary dark button"
                    , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
                    ]
                    [ text activeClinicName ]
                ]

        activeClinicName =
            Backend.Session.Utils.activeClinicName session
                |> Maybe.withDefault (translate language Translate.ClinicNotFound)

        clinicName =
            EveryDictList.get clinicId session.offlineSession.clinics
                |> Maybe.map .name
                |> Maybe.withDefault (translate language Translate.ClinicNotFound)
    in
    div [ class "wrap wrap-alt-2" ]
        [ viewDownloadProgress language backend.offlineSessionRequest cache.cacheStorage.cachedPhotos sessionId
        , viewUploadProgress language backend.uploadEditsRequest
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
