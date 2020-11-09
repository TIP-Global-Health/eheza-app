module Pages.Clinics.View exposing (view)

{-| The purpose of this page is to show a list of clinics, allowing the
user to click on clinics the user is assigned to, to see the sessions which are
available for data-entry.
-}

import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (Clinic, ClinicType(..), allClinicTypes)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isAuthorithedNurse)
import Backend.Session.Model exposing (Session)
import Backend.Session.Utils exposing (isClosed)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Clinics.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData, isLoading)
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model
import SyncManager.Utils exposing (getSyncedHealthCenters)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


{-| If `selectedClinic` is Just, we'll show a page for that clinic. If not,
we'll show a list of clinics.

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

-}
view : Language -> NominalDate -> Nurse -> HealthCenterId -> Maybe ClinicId -> Model -> ModelIndexedDb -> SyncManager.Model.Model -> Html Msg
view language currentDate user healthCenterId selectedClinic model db syncManager =
    case selectedClinic of
        Just clinicId ->
            viewClinic language currentDate user clinicId db

        Nothing ->
            viewClinicList language user healthCenterId model db syncManager


viewClinicList : Language -> Nurse -> HealthCenterId -> Model -> ModelIndexedDb -> SyncManager.Model.Model -> Html Msg
viewClinicList language user healthCenterId model db syncManager =
    let
        syncedHealthCenters =
            getSyncedHealthCenters syncManager
                |> List.map toEntityUuid

        content =
            viewWebData language
                (viewLoadedClinicList language user healthCenterId syncedHealthCenters model)
                identity
                db.clinics

        ( titleTransId, goBackAction ) =
            if isJust model.clinicType then
                ( Translate.Groups, SetClinicType Nothing )

            else
                ( Translate.Programs, SetActivePage PinCodePage )
    in
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1 [ class "ui header" ]
                [ text <| translate language titleTransId ]
            , a
                [ class "link-back"
                , onClick goBackAction
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
viewLoadedClinicList : Language -> Nurse -> HealthCenterId -> List HealthCenterId -> Model -> Dict ClinicId Clinic -> Html Msg
viewLoadedClinicList language user selectedHealthCenterId syncedHealthCenters model clinics =
    let
        showWarningMessage header message =
            div
                [ class "ui message warning" ]
                [ div [ class "header" ] [ text <| translate language header ]
                , text <| translate language message
                ]

        -- @todo : revise if this is needed or not.
        selectedHealthCenterSynced =
            True

        isDownloading =
            False

        isUploading =
            False
    in
    if not selectedHealthCenterSynced then
        showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync

    else if isDownloading then
        showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCDownloading

    else if isUploading then
        showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCUploading

    else
        let
            titleTransId =
                if isJust model.clinicType then
                    Translate.SelectYourGroup

                else
                    Translate.SelectProgram

            title =
                p
                    [ class "centered" ]
                    [ text <| translate language titleTransId
                    , text ":"
                    ]

            synced =
                case model.clinicType of
                    Just clinicType ->
                        clinics
                            |> Dict.filter
                                (\_ clinic ->
                                    -- Group belongs to seleced health center.
                                    (clinic.healthCenterId == selectedHealthCenterId)
                                        -- Health center is synced.
                                        && List.member clinic.healthCenterId syncedHealthCenters
                                        -- Group is of selected type.
                                        && (clinic.clinicType == clinicType)
                                )
                            |> Dict.toList
                            |> List.sortBy (Tuple.second >> .name)
                            |> Dict.fromList

                    Nothing ->
                        clinics
                            |> Dict.filter
                                (\_ clinic ->
                                    -- Group belongs to seleced health center.
                                    (clinic.healthCenterId == selectedHealthCenterId)
                                        -- Health center is synced.
                                        && List.member clinic.healthCenterId syncedHealthCenters
                                )

            buttonsView =
                if isJust model.clinicType then
                    synced
                        |> Dict.toList
                        |> List.map (viewClinicButton user)

                else
                    synced
                        |> Dict.values
                        |> viewClinicTypeButtons language
        in
        div []
            [ title
            , div [] buttonsView
            ]


viewClinicButton : Nurse -> ( ClinicId, Clinic ) -> Html Msg
viewClinicButton nurse ( clinicId, clinic ) =
    let
        classAttr =
            if isAuthorithedNurse clinic nurse then
                class "ui fluid primary button"

            else
                class "ui fluid primary dark disabled button"
    in
    button
        [ classAttr
        , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just clinicId
        ]
        [ text clinic.name ]


viewClinicTypeButtons : Language -> List Clinic -> List (Html Msg)
viewClinicTypeButtons language clinics =
    let
        clinicsTypes =
            clinics
                |> List.map .clinicType

        allowedTypes =
            allClinicTypes
                |> List.filter (\type_ -> List.member type_ clinicsTypes)
    in
    allowedTypes
        |> List.map
            (\allowedType ->
                button
                    [ class "ui fluid primary button"
                    , onClick <| SetClinicType (Just allowedType)
                    ]
                    [ text <| translate language (Translate.ClinicType allowedType) ]
            )


{-| View a specific clinic.
-}
viewClinic : Language -> NominalDate -> Nurse -> ClinicId -> ModelIndexedDb -> Html Msg
viewClinic language currentDate nurse clinicId db =
    let
        clinic =
            RemoteData.map (Dict.get clinicId) db.clinics

        sessions =
            db.sessionsByClinic
                |> Dict.get clinicId
                |> Maybe.withDefault NotAsked
    in
    viewWebData language
        (viewLoadedClinic language currentDate nurse db.postSession clinicId)
        identity
        (RemoteData.append clinic sessions)


viewLoadedClinic : Language -> NominalDate -> Nurse -> WebData SessionId -> ClinicId -> ( Maybe Clinic, Dict SessionId Session ) -> Html Msg
viewLoadedClinic language currentDate nurse postSession clinicId ( clinic, sessions ) =
    case clinic of
        Just clinic_ ->
            div
                [ class "wrap wrap-alt-2" ]
                (viewFoundClinic language currentDate nurse postSession clinicId clinic_ sessions)

        Nothing ->
            Pages.PageNotFound.View.viewPage language
                (SetActivePage <| UserPage <| ClinicsPage Nothing)
                (UserPage <| ClinicsPage <| Just clinicId)


{-| We show recent and upcoming sessions, with a link to dive into the session
if it is open. (That is, the dates are correct and it's not explicitly closed).
We'll show anything which was scheduled to start or end within the last week
or the next week.
-}
viewFoundClinic : Language -> NominalDate -> Nurse -> WebData SessionId -> ClinicId -> Clinic -> Dict SessionId Session -> List (Html Msg)
viewFoundClinic language currentDate nurse postSession clinicId clinic sessions =
    let
        daysToShow =
            7

        recentAndUpcomingSessions =
            sessions
                |> Dict.filter
                    (\_ session ->
                        let
                            deltaToEndDateDays =
                                session.endDate
                                    |> Maybe.withDefault currentDate
                                    |> (\endDate -> Date.diff Days endDate currentDate)

                            deltaToStartDateDays =
                                Date.diff Days session.startDate currentDate
                        in
                        -- Ends last week or next week
                        (abs deltaToEndDateDays <= daysToShow)
                            || -- Starts last week or next week
                               (abs deltaToStartDateDays <= daysToShow)
                            || -- Is between start and end date
                               (deltaToStartDateDays <= 0 && deltaToEndDateDays >= 0)
                    )

        sessionsStartedToday =
            recentAndUpcomingSessions
                |> Dict.filter (\_ session -> session.startDate == currentDate)

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
            Dict.isEmpty sessionsStartedToday

        defaultSession =
            { startDate = currentDate
            , endDate = Nothing
            , clinicId = clinicId
            , clinicType = clinic.clinicType
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
                    |> MsgIndexedDb
                    |> onClick
                ]
                [ text <| translate language Translate.CreateGroupEncounter ]

        content =
            if isAuthorithedNurse clinic nurse then
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
                        |> Dict.map (viewSession language currentDate)
                        |> Dict.values
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
        [ td [] [ text <| formatDDMMYYYY session.startDate ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map formatDDMMYYYY session.endDate ]
        , td [] [ link ]
        ]
