module Pages.Clinics.View exposing (view)

{-| The purpose of this page is to show a list of clinics, allowing the
user to click on clinics the user is assigned to, to see the sessions which are
available for data-entry.
-}

import AllDict
import AllDictList
import App.Model exposing (Msg(..), MsgLoggedIn(..))
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (assignedToHealthCenter)
import Backend.Session.Model exposing (Session)
import Backend.Session.Utils exposing (isClosed)
import Backend.SyncData.Model exposing (SyncData)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData, isLoading)
import Time.Date exposing (delta)
import Translate exposing (Language, translate)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)
import Utils.WebData exposing (viewError, viewWebData)


{-| If `selectedClinic` is Just, we'll show a page for that clinic. If not,
we'll show a list of clinics.

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

-}
view : Language -> NominalDate -> Nurse -> HealthCenterId -> Maybe ClinicId -> ModelIndexedDb -> Html Msg
view language currentDate user healthCenterId selectedClinic db =
    case selectedClinic of
        Just clinicId ->
            viewClinic language currentDate user clinicId db

        Nothing ->
            viewClinicList language user healthCenterId db


viewClinicList : Language -> Nurse -> HealthCenterId -> ModelIndexedDb -> Html Msg
viewClinicList language user healthCenterId db =
    let
        content =
            viewWebData language
                (viewLoadedClinicList language user healthCenterId)
                identity
                (RemoteData.append db.clinics db.syncData)
    in
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1 [ class "ui header" ]
                [ text <| translate language Translate.Groups ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage PinCodePage
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui basic segment" ]
            [ content ]
        ]


{-| This is the "inner" view function ... we get here if all the data was
actually available.

We only show clinics for the health centers that we are syncing. In principle,
we could show something about the sync status here ... might want to know how
up-to-date things are.

-}
viewLoadedClinicList : Language -> Nurse -> HealthCenterId -> ( EntityUuidDictList ClinicId Clinic, EntityUuidDictList HealthCenterId SyncData ) -> Html Msg
viewLoadedClinicList language user selectedHealthCenterId ( clinics, sync ) =
    let
        title =
            p
                [ class "centered" ]
                [ text <| translate language Translate.SelectYourGroup
                , text ":"
                ]

        synced =
            clinics
                |> AllDictList.filter
                    (\_ clinic ->
                        -- Group belongs to seleced health center.
                        (clinic.healthCenterId == selectedHealthCenterId)
                            -- Health center is synced.
                            && AllDictList.member clinic.healthCenterId sync
                    )
                |> AllDictList.sortBy .name

        clinicView =
            synced
                |> AllDictList.toList
                |> List.map (viewClinicButton user)

        message =
            if AllDictList.isEmpty synced then
                div
                    [ class "ui message warning" ]
                    [ div [ class "header" ] [ text <| translate language Translate.NoGroupsFound ]
                    , text <| translate language Translate.HaveYouSynced
                    ]

            else
                emptyNode
    in
    div []
        [ title
        , div [] clinicView
        , message
        ]


viewClinicButton : Nurse -> ( ClinicId, Clinic ) -> Html Msg
viewClinicButton user ( clinicId, clinic ) =
    let
        classAttr =
            if assignedToHealthCenter clinic.healthCenterId user then
                class "ui fluid primary button"

            else
                class "ui fluid primary dark disabled button"
    in
    button
        [ classAttr
        , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just clinicId
        ]
        [ text clinic.name ]


{-| View a specific clinic.
-}
viewClinic : Language -> NominalDate -> Nurse -> ClinicId -> ModelIndexedDb -> Html Msg
viewClinic language currentDate nurse clinicId db =
    let
        clinic =
            RemoteData.map (AllDictList.get clinicId) db.clinics

        sessions =
            db.sessionsByClinic
                |> AllDict.get clinicId
                |> Maybe.withDefault NotAsked
    in
    viewWebData language
        (viewLoadedClinic language currentDate nurse db.postSession clinicId)
        identity
        (RemoteData.append clinic sessions)


viewLoadedClinic : Language -> NominalDate -> Nurse -> WebData SessionId -> ClinicId -> ( Maybe Clinic, EntityUuidDictList SessionId Session ) -> Html Msg
viewLoadedClinic language currentDate nurse postSession clinicId ( clinic, sessions ) =
    case clinic of
        Just clinic ->
            div
                [ class "wrap wrap-alt-2" ]
                (viewFoundClinic language currentDate nurse postSession clinicId clinic sessions)

        Nothing ->
            Pages.PageNotFound.View.viewPage language
                (SetActivePage <| UserPage <| ClinicsPage Nothing)
                (UserPage <| ClinicsPage <| Just clinicId)


{-| We show recent and upcoming sessions, with a link to dive into the session
if it is open. (That is, the dates are correct and it's not explicitly closed).
We'll show anything which was scheduled to start or end within the last week
or the next week.
-}
viewFoundClinic : Language -> NominalDate -> Nurse -> WebData SessionId -> ClinicId -> Clinic -> EntityUuidDictList SessionId Session -> List (Html Msg)
viewFoundClinic language currentDate nurse postSession clinicId clinic sessions =
    let
        daysToShow =
            7

        recentAndUpcomingSessions =
            sessions
                |> AllDictList.filter
                    (\sessionId session ->
                        let
                            deltaToEndDate =
                                session.endDate
                                    |> Maybe.withDefault currentDate
                                    |> (\endDate -> delta endDate currentDate)

                            deltaToStartDate =
                                delta session.startDate currentDate
                        in
                        -- Ends last week or next week
                        (abs deltaToEndDate.days <= daysToShow)
                            || -- Starts last week or next week
                               (abs deltaToStartDate.days <= daysToShow)
                            || -- Is between start and end date
                               (deltaToStartDate.days <= 0 && deltaToEndDate.days >= 0)
                    )

        sessionsStartedToday =
            recentAndUpcomingSessions
                |> AllDictList.filter (\_ session -> session.startDate == currentDate)

        -- We allow the creation of a new session if there is no session that
        -- was started today. So, there are several scenarios:
        --
        -- 1. If there are open sessions from the past (not started today), you
        --    can choose one of them, or start a new session.
        --
        -- 2. If there is an open session started today, you can only choose
        --    that session. You can't start a second open session for today.
        --
        -- Note that we can theoretically end up with two sessions started the
        -- same day if they were created on different devices, and the second
        -- is created before the first syncs. We could write some code to
        -- automatically "consolidate" those two sessions.
        enableCreateSessionButton =
            AllDictList.isEmpty sessionsStartedToday

        defaultSession =
            { startDate = currentDate
            , endDate = Nothing
            , clinicId = clinicId
            }

        createSessionButton =
            button
                [ classList
                    [ ( "ui button", True )
                    , ( "disabled", not enableCreateSessionButton )
                    , ( "active", enableCreateSessionButton )
                    , ( "loading", isLoading postSession )
                    ]
                , defaultSession
                    |> PostSession
                    |> App.Model.MsgIndexedDb
                    |> onClick
                ]
                [ text <| translate language Translate.CreateGroupEncounter ]

        content =
            if assignedToHealthCenter clinic.healthCenterId nurse then
                [ h1 [] [ text <| translate language Translate.RecentAndUpcomingGroupEncounters ]
                , table
                    [ class "ui table session-list" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text <| translate language Translate.StartDate ]
                            , th [] [ text <| translate language Translate.EndDate ]
                            ]
                        ]
                    , recentAndUpcomingSessions
                        |> AllDictList.map (viewSession language currentDate)
                        |> AllDictList.values
                        |> tbody []
                    ]
                , createSessionButton
                ]

            else
                [ div [ class "ui message error" ]
                    [ text <| translate language Translate.GroupUnauthorized ]
                ]
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
    , div [ class "ui basic segment" ] content
    ]


viewSession : Language -> NominalDate -> SessionId -> Session -> Html Msg
viewSession language currentDate sessionId session =
    let
        enableLink =
            not (isClosed currentDate session)

        link =
            button
                [ classList
                    [ ( "ui button", True )
                    , ( "disabled", not enableLink )
                    , ( "active", enableLink )
                    ]
                , SessionPage sessionId AttendancePage
                    |> UserPage
                    |> SetActivePage
                    |> onClick
                ]
                [ text <| translate language Translate.Attendance ]
    in
    tr []
        [ td [] [ text <| formatYYYYMMDD session.startDate ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map formatYYYYMMDD session.endDate ]
        , td [] [ link ]
        ]
