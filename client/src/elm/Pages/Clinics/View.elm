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
import Backend.NutritionEncounter.Utils exposing (sortByDate)
import Backend.Session.Model exposing (Session)
import Backend.Session.Utils exposing (isClosed)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import List.Zipper as Zipper
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Clinics.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData, isLoading)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model exposing (SyncInfoStatus(..))
import SyncManager.Utils exposing (getSyncedHealthCenters)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


{-| If `selectedClinic` is Just, we'll show a page for that clinic. If not,
we'll show a list of clinics.

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

-}
view : Language -> NominalDate -> Nurse -> HealthCenterId -> Model -> ModelIndexedDb -> SyncManager.Model.Model -> Html Msg
view language currentDate user healthCenterId model db syncManager =
    let
        content =
            viewWebData language
                (viewLoadedClinicList language currentDate user healthCenterId syncManager db model)
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
            , span
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
viewLoadedClinicList : Language -> NominalDate -> Nurse -> HealthCenterId -> SyncManager.Model.Model -> ModelIndexedDb -> Model -> Dict ClinicId Clinic -> Html Msg
viewLoadedClinicList language currentDate user selectedHealthCenterId syncManager db model clinics =
    let
        syncedHealthCenters =
            getSyncedHealthCenters syncManager
                |> List.map toEntityUuid

        showWarningMessage header message =
            div
                [ class "ui message warning" ]
                [ div [ class "header" ] [ text <| translate language header ]
                , text <| translate language message
                ]

        selectedHealthCenterSyncInfo =
            syncManager.syncInfoAuthorities
                |> Maybe.andThen
                    (Zipper.toList >> List.Extra.find (\authorityInfo -> authorityInfo.uuid == fromEntityUuid selectedHealthCenterId))
    in
    selectedHealthCenterSyncInfo
        |> Maybe.map
            (\syncInfo ->
                case syncInfo.status of
                    NotAvailable ->
                        showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync

                    Uploading ->
                        showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCUploading

                    Downloading ->
                        showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCDownloading

                    _ ->
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
                                    Dict.toList synced
                                        |> List.map (viewClinicButton currentDate user db)

                                else
                                    Dict.values synced
                                        |> viewClinicTypeButtons language
                        in
                        div []
                            [ title
                            , div [] buttonsView
                            ]
            )
        |> Maybe.withDefault
            (showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync)


viewClinicButton : NominalDate -> Nurse -> ModelIndexedDb -> ( ClinicId, Clinic ) -> Html Msg
viewClinicButton currentDate nurse db ( clinicId, clinic ) =
    let
        attributes =
            if isAuthorithedNurse clinic nurse then
                let
                    sessions =
                        db.sessionsByClinic
                            |> Dict.get clinicId
                            |> Maybe.andThen RemoteData.toMaybe

                    sessionStartedToday =
                        Maybe.andThen
                            (Dict.filter (\_ session -> session.startDate == currentDate)
                                >> Dict.toList
                                >> List.sortWith (sortByDate (Tuple.second >> .startDate))
                                >> List.head
                            )
                            sessions

                    action =
                        Maybe.map
                            (\( sessionId, _ ) ->
                                [ SessionPage sessionId AttendancePage
                                    |> UserPage
                                    |> SetActivePage
                                    |> onClick
                                ]
                            )
                            sessionStartedToday
                            |> Maybe.withDefault
                                [ { startDate = currentDate
                                  , endDate = Nothing
                                  , clinicId = clinicId
                                  , clinicType = clinic.clinicType
                                  }
                                    |> PostSession
                                    |> MsgIndexedDb
                                    |> onClick
                                ]
                in
                class "ui fluid primary button" :: action

            else
                [ class "ui fluid primary dark disabled button" ]
    in
    button attributes
        [ text clinic.name ]


viewClinicTypeButtons : Language -> List Clinic -> List (Html Msg)
viewClinicTypeButtons language clinics =
    let
        clinicsTypes =
            List.map .clinicType clinics

        allowedTypes =
            List.filter (\type_ -> List.member type_ clinicsTypes)
                allClinicTypes
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
