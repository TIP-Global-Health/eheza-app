module SyncManager.View exposing (view)

import App.Model exposing (ConfiguredModel)
import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Model exposing (ModelIndexedDb)
import Editable
import Gizra.Html exposing (emptyNode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Encode
import List.Extra
import List.Zipper as Zipper
import Maybe.Extra exposing (isJust)
import RemoteData exposing (RemoteData, WebData)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendGeneralEntity(..)
        , DownloadPhotosBatchRec
        , DownloadPhotosMode(..)
        , DownloadSyncResponse
        , Model
        , Msg(..)
        , SyncCycle(..)
        , SyncStatus(..)
        )
import SyncManager.Utils exposing (getSyncedHealthCenters)
import Translate exposing (Language, translate)
import Url
import Utils.Html exposing (spinner)
import Utils.WebData


view : Language -> RemoteData String ConfiguredModel -> ModelIndexedDb -> Model -> Html Msg
view language configuredModel db model =
    case RemoteData.toMaybe configuredModel of
        Just configuration ->
            if not configuration.config.debug then
                emptyNode

            else
                details
                    [ property "open" (Json.Encode.bool False)
                    , class "sync-manager"
                    ]
                    [ summary [] [ text "Sync Manager" ]
                    , viewHealthCentersForSync language db model
                    , viewSyncSettings language model
                    , viewSyncStatus language db model
                    ]

        Nothing ->
            emptyNode


{-| Helper to see the device UUID, and current nurse, if logged in.
-}
viewDeviceInfo : Language -> ConfiguredModel -> Html Msg
viewDeviceInfo language configuration =
    let
        loggedInNurse =
            case RemoteData.toMaybe configuration.loggedIn of
                Just loggedIn ->
                    let
                        nurse =
                            Tuple.second loggedIn.nurse
                    in
                    div [] [ text <| "Nurse: " ++ nurse.name ]

                Nothing ->
                    div [] [ text "Nurse not logged in" ]

        deviceIdInfo =
            case RemoteData.toMaybe configuration.device of
                Just device ->
                    case device.deviceId of
                        Just deviceId ->
                            div [] [ text <| "Device ID: " ++ String.fromInt deviceId ]

                        Nothing ->
                            div []
                                [ text """
                                    Device ID not set as device was paired before June 2020.
                                    You may re-pair to get a new ID, but in general this is not a problem -
                                    it just makes troubleshooting a bit easier.
                                    """
                                ]

                Nothing ->
                    div [] [ text "Device ID not set, either not paired, or a pair before June 2020" ]
    in
    details
        [ class "segment ui"
        , property "open" (Json.Encode.bool False)
        ]
        [ summary [] [ text "Device info" ]
        , loggedInNurse
        , deviceIdInfo
        ]


viewSyncSettings : Language -> Model -> Html Msg
viewSyncSettings language model =
    let
        currentStatus =
            case model.syncCycle of
                SyncCycleOn ->
                    "Cycle is currently on"

                SyncCycleStayOnCurrentSyncStatus ->
                    "Cycle is currently off, sync continues, but won't rotate to a new sync step"

                SyncCyclePause ->
                    "Sync is completely paused"

        getOption status =
            case status of
                SyncCycleOn ->
                    { name = "On"
                    , icon = "recycle"
                    , msg = SetSyncCycle SyncCycleOn
                    }

                SyncCycleStayOnCurrentSyncStatus ->
                    { name = "Stay on current sync step"
                    , icon = "repeat"
                    , msg = SetSyncCycle SyncCycleStayOnCurrentSyncStatus
                    }

                SyncCyclePause ->
                    { name = "Pause"
                    , icon = "pause"
                    , msg = SetSyncCycle SyncCyclePause
                    }

        syncCycleButtons =
            div []
                (List.map
                    (\status ->
                        let
                            record =
                                getOption status
                        in
                        button
                            [ classList
                                [ ( "ui labeled icon button", True )
                                , ( "active", model.syncCycle == status )
                                ]
                            , onClick record.msg
                            ]
                            [ i [ class <| "icon " ++ record.icon ] []
                            , text record.name
                            ]
                    )
                    [ SyncCycleOn, SyncCycleStayOnCurrentSyncStatus, SyncCyclePause ]
                )

        syncSpeed =
            Editable.value model.syncSpeed
    in
    details
        [ property "open" (Json.Encode.bool False)
        , class "segment ui"
        ]
        [ summary [] [ text "Sync Settings" ]
        , fieldset []
            [ div [] [ text "Sync Speed" ]
            , div [ class "ui right labeled input fluid" ]
                [ label [ class "ui label" ] [ text "Idle time" ]
                , input
                    [ type_ "number"

                    -- No less than every 3 second. On production it should be
                    -- no less than 10 seconds.
                    , Html.Attributes.min (String.fromInt <| 3 * 1000)

                    -- No more than every 5 minutes.
                    , Html.Attributes.max (String.fromInt <| 5 * 60 * 1000)
                    , Html.Attributes.required True
                    , value <| String.fromInt syncSpeed.idle
                    , onInput SetSyncSpeedIdle
                    ]
                    []
                , div [ class "ui basic label" ] [ text "ms" ]
                ]
            , div [ class "ui right labeled input fluid" ]
                [ label [ class "ui label" ] [ text "Cycle time" ]
                , input
                    [ type_ "number"

                    -- No less than every 50 ms.
                    , Html.Attributes.min (String.fromInt <| 50)

                    -- No more than every 5 minutes.
                    , Html.Attributes.max (String.fromInt <| 5 * 60 * 1000)
                    , Html.Attributes.required True
                    , value <| String.fromInt syncSpeed.cycle
                    , onInput SetSyncSpeedCycle
                    ]
                    []
                , div [ class "ui basic label" ] [ text "ms" ]
                ]
            , div [ class "ui right labeled input fluid" ]
                [ label [ class "ui label" ] [ text "Offline time" ]
                , input
                    [ type_ "number"

                    -- No less than every 1000 ms.
                    , Html.Attributes.min (String.fromInt <| 1000)

                    -- No more than every 5 minutes.
                    , Html.Attributes.max (String.fromInt <| 5 * 60 * 1000)
                    , Html.Attributes.required True
                    , value <| String.fromInt syncSpeed.offline
                    , onInput SetSyncSpeedOffline
                    ]
                    []
                , div [ class "ui basic label" ] [ text "ms" ]
                ]
            , div []
                [ button
                    [ onClick SaveSettings
                    , class "ui primary button"
                    ]
                    [ text "Save" ]
                , button
                    [ onClick ResetSettings
                    , class "ui button"
                    ]
                    [ text "Reset Settings" ]
                ]
            ]
        , fieldset []
            [ div [] [ text "Sync Cycle" ]
            , div [] [ text <| "Current status: " ++ currentStatus ]
            , syncCycleButtons
            ]
        ]


viewSyncDownloadGeneral : Language -> Model -> WebData (DownloadSyncResponse BackendGeneralEntity) -> Html Msg
viewSyncDownloadGeneral language model webData =
    div []
        [ div [] [ text <| "Fetch from General from revision ID " ++ String.fromInt model.syncInfoGeneral.lastFetchedRevisionId ]
        , button [ onClick <| SyncManager.Model.SetLastFetchedRevisionIdGeneral 0 ] [ text "Reset revision ID to 0" ]
        , div [] [ text "HTTP requests:" ]
        , case webData of
            RemoteData.Success data ->
                div []
                    [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                    , if List.isEmpty data.entities then
                        div [] [ text "No content fetched in last HTTP request" ]

                      else
                        ol [] (List.map (viewGeneralEntity language) data.entities)
                    ]

            RemoteData.Failure error ->
                text <| Debug.toString error

            RemoteData.Loading ->
                spinner

            RemoteData.NotAsked ->
                emptyNode
        ]


viewGeneralEntity : Language -> BackendGeneralEntity -> Html msg
viewGeneralEntity language backendGeneralEntity =
    li []
        [ case backendGeneralEntity of
            BackendGeneralCatchmentArea identifier ->
                text <| "Catchment area " ++ identifier.entity.name

            BackendGeneralCounselingSchedule identifier ->
                text <| "Counseling Schedule " ++ identifier.uuid

            BackendGeneralCounselingTopic identifier ->
                text <| "Counseling Topic " ++ identifier.entity.english

            BackendGeneralHealthCenter identifier ->
                text <| "Health Center " ++ identifier.entity.name

            BackendGeneralNurse identifier ->
                text <| "Nurse " ++ identifier.entity.name

            BackendGeneralParticipantForm identifier ->
                text <| "Participant Form " ++ identifier.entity.title.english

            BackendGeneralVillage identifier ->
                text <| "Village " ++ identifier.entity.name
        ]


viewSyncDownloadAuthority : Language -> ModelIndexedDb -> Model -> WebData (DownloadSyncResponse BackendAuthorityEntity) -> Html Msg
viewSyncDownloadAuthority language db model webData =
    case model.syncInfoAuthorities of
        Nothing ->
            emptyNode

        Just zipper ->
            let
                currentZipper =
                    Zipper.current zipper

                getAuthorityName uuid =
                    db.healthCenters
                        |> RemoteData.toMaybe
                        |> Maybe.andThen (\healthCenters -> Dict.get (toEntityUuid uuid) healthCenters)
                        |> Maybe.map (\healthCenter -> healthCenter.name)
                        |> Maybe.withDefault uuid

                authoritiesListHtml =
                    Zipper.toList zipper
                        |> List.map
                            (\row ->
                                if row.uuid == currentZipper.uuid then
                                    li [ class "active" ] [ text <| getAuthorityName row.uuid ++ " (from revision ID " ++ String.fromInt row.lastFetchedRevisionId ++ ")" ]

                                else
                                    li [] [ text <| getAuthorityName row.uuid ]
                            )
            in
            div []
                [ div [] [ text <| "Fetch from Authority" ]
                , ol [] authoritiesListHtml
                , button [ onClick <| SyncManager.Model.SetLastFetchedRevisionIdAuthority zipper 0 ] [ text "Reset revision ID to 0" ]
                , case webData of
                    RemoteData.Success data ->
                        div []
                            [ div [] [ text <| String.fromInt data.revisionCount ++ " items left to download" ]
                            , if List.isEmpty data.entities then
                                div [] [ text "No content fetched in last HTTP request" ]

                              else
                                div []
                                    [ div [] [ text <| "Here is the content we've fetched in the last HTTP request:" ]
                                    , ol [] (List.map viewAuthorityEntity data.entities)
                                    ]
                            ]

                    RemoteData.Failure error ->
                        text <| Debug.toString error

                    RemoteData.Loading ->
                        spinner

                    RemoteData.NotAsked ->
                        emptyNode
                ]


viewAuthorityEntity : BackendAuthorityEntity -> Html msg
viewAuthorityEntity backendAuthorityEntity =
    let
        viewMeasurement identifier name =
            text (name ++ " for person ID " ++ fromEntityUuid identifier.entity.participantId)
    in
    li []
        [ case backendAuthorityEntity of
            BackendAuthorityAcuteFindings identifier ->
                viewMeasurement identifier "Acute Findings"

            BackendAuthorityAcuteIllnessEncounter identifier ->
                text ("Acute Illness Encounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityAcuteIllnessMuac identifier ->
                viewMeasurement identifier "Acute Illness Muac"

            BackendAuthorityAcuteIllnessNutrition identifier ->
                viewMeasurement identifier "Acute Illness Nutrition"

            BackendAuthorityAcuteIllnessVitals identifier ->
                viewMeasurement identifier "Acute Illness Vitals"

            BackendAuthorityAttendance identifier ->
                viewMeasurement identifier "Attendance"

            BackendAuthorityBreastExam identifier ->
                viewMeasurement identifier "Breast Exam"

            BackendAuthorityCall114 identifier ->
                viewMeasurement identifier "Call 114"

            BackendAuthorityChildFbf identifier ->
                viewMeasurement identifier "Child Fbf"

            BackendAuthorityClinic identifier ->
                text <| "Clinic " ++ identifier.entity.name

            BackendAuthorityCounselingSession identifier ->
                viewMeasurement identifier "Counseling Session"

            BackendAuthorityCorePhysicalExam identifier ->
                viewMeasurement identifier "Core Physical Exam"

            BackendAuthorityDangerSigns identifier ->
                viewMeasurement identifier "Danger Signs"

            BackendAuthorityDashboardStats identifier ->
                text "Dashboard Statistics"

            BackendAuthorityExposure identifier ->
                viewMeasurement identifier "Exposure"

            BackendAuthorityFamilyPlanning identifier ->
                viewMeasurement identifier "Family Planning"

            BackendAuthorityHCContact identifier ->
                viewMeasurement identifier "HC Contact"

            BackendAuthorityHeight identifier ->
                viewMeasurement identifier "Height"

            BackendAuthorityIndividualParticipant identifier ->
                text <| "Individual Participant for person ID " ++ fromEntityUuid identifier.entity.person

            BackendAuthorityIsolation identifier ->
                viewMeasurement identifier "Isolation"

            BackendAuthorityLactation identifier ->
                viewMeasurement identifier "Lactation"

            BackendAuthorityMalariaTesting identifier ->
                viewMeasurement identifier "Malaria Testing"

            BackendAuthorityLastMenstrualPeriod identifier ->
                viewMeasurement identifier "Menstrual Period"

            BackendAuthorityMedicalHistory identifier ->
                viewMeasurement identifier "Medical History"

            BackendAuthorityMedication identifier ->
                viewMeasurement identifier "Medication"

            BackendAuthorityMedicationDistribution identifier ->
                viewMeasurement identifier "Medication Distribution"

            BackendAuthorityMotherFbf identifier ->
                viewMeasurement identifier "Mother Fbf"

            BackendAuthorityMuac identifier ->
                viewMeasurement identifier "Muac"

            BackendAuthorityNutrition identifier ->
                viewMeasurement identifier "Nutrition"

            BackendAuthorityNutritionEncounter identifier ->
                text ("NutritionEncounter for participant ID " ++ fromEntityUuid identifier.entity.participant)

            BackendAuthorityNutritionHeight identifier ->
                viewMeasurement identifier "Nutrition Height"

            BackendAuthorityNutritionMuac identifier ->
                viewMeasurement identifier "Nutrition Muac"

            BackendAuthorityNutritionNutrition identifier ->
                viewMeasurement identifier "Nutrition Nutrition"

            BackendAuthorityNutritionPhoto identifier ->
                viewMeasurement identifier "Nutrition Photo"

            BackendAuthorityNutritionWeight identifier ->
                viewMeasurement identifier "Nutrition Weight"

            BackendAuthorityObstetricHistory identifier ->
                viewMeasurement identifier "Obstetric History"

            BackendAuthorityObstetricHistoryStep2 identifier ->
                viewMeasurement identifier "Obstetric History Step 2"

            BackendAuthorityObstetricalExam identifier ->
                viewMeasurement identifier "Obstetrical Exam"

            BackendAuthorityParticipantConsent identifier ->
                viewMeasurement identifier "Participant Consent"

            BackendAuthorityPerson identifier ->
                text <| "Person " ++ identifier.entity.name

            BackendAuthorityPhoto identifier ->
                viewMeasurement identifier "Photo"

            BackendAuthorityPrenatalPhoto identifier ->
                viewMeasurement identifier "Prenatal Photo"

            BackendAuthorityPmtctParticipant identifier ->
                text <| "Pmtct Participant for child ID " ++ fromEntityUuid identifier.entity.child

            BackendAuthorityPrenatalFamilyPlanning identifier ->
                viewMeasurement identifier "Prenatal Family Planning"

            BackendAuthorityPrenatalNutrition identifier ->
                viewMeasurement identifier "Prenatal Nutrition"

            BackendAuthorityPrenatalEncounter identifier ->
                text <| "Prenatal Encounter for person ID " ++ fromEntityUuid identifier.entity.participant

            BackendAuthorityRelationship identifier ->
                text <| "Relationship for person ID " ++ fromEntityUuid identifier.entity.person

            BackendAuthorityResource identifier ->
                viewMeasurement identifier "Resource"

            BackendAuthoritySendToHC identifier ->
                viewMeasurement identifier "Send to HC"

            BackendAuthoritySession identifier ->
                text <| "Session for Clinic ID " ++ fromEntityUuid identifier.entity.clinicId

            BackendAuthoritySocialHistory identifier ->
                viewMeasurement identifier "Social History"

            BackendAuthoritySymptomsGeneral identifier ->
                viewMeasurement identifier "Symptoms General"

            BackendAuthoritySymptomsGI identifier ->
                viewMeasurement identifier "Symptoms GI"

            BackendAuthoritySymptomsRespiratory identifier ->
                viewMeasurement identifier "Symptoms Respiratory"

            BackendAuthorityTravelHistory identifier ->
                viewMeasurement identifier "Travel History"

            BackendAuthorityTreatmentOngoing identifier ->
                viewMeasurement identifier "Treatment Ongoing"

            BackendAuthorityTreatmentReview identifier ->
                viewMeasurement identifier "Treatment Review"

            BackendAuthorityVitals identifier ->
                viewMeasurement identifier "Vitals"

            BackendAuthorityWeight identifier ->
                viewMeasurement identifier "Weight"
        ]


viewDownloadPhotosBatch : Language -> Model -> DownloadPhotosBatchRec -> Html Msg
viewDownloadPhotosBatch language model deferredPhoto =
    case deferredPhoto.indexDbRemoteData of
        RemoteData.Success (Just result) ->
            let
                fileName =
                    result.photo
                        |> Url.fromString
                        |> Maybe.andThen
                            (\url ->
                                url.path
                                    |> String.split "/"
                                    |> List.Extra.last
                            )
                        |> Maybe.withDefault ""

                attempt =
                    result.attempts + 1

                attemptString =
                    case attempt of
                        1 ->
                            "1st"

                        2 ->
                            "2nd"

                        3 ->
                            "3rd"

                        _ ->
                            String.fromInt attempt ++ "th"
            in
            div []
                [ text <|
                    "Photos batch download ("
                        ++ String.fromInt (deferredPhoto.batchCounter + 1)
                        ++ " out of "
                        ++ String.fromInt deferredPhoto.batchSize
                        ++ ")"
                , div []
                    [ text <| attemptString ++ " attempt to download "
                    , a [ href result.photo, target "_blank" ] [ text fileName ]
                    ]
                ]

        _ ->
            emptyNode


{-| Show a list of Authorities that allow syncing from.
-}
viewHealthCentersForSync : Language -> ModelIndexedDb -> Model -> Html Msg
viewHealthCentersForSync language db model =
    let
        -- The Health centers that are synced.
        selectedHealthCentersUuid =
            getSyncedHealthCenters model
    in
    case db.healthCenters of
        RemoteData.Success healthCenters ->
            if Dict.isEmpty healthCenters then
                details [ class "segment ui" ]
                    [ summary [] [ text "No health centers synced yet" ] ]

            else
                details
                    [ class "segment ui"
                    , property "open" (Json.Encode.bool False)
                    ]
                    [ summary [] [ text "Health Centers" ]
                    , ul []
                        (List.map
                            (\( healthCenterId, healthCenter ) ->
                                let
                                    isSynced =
                                        List.Extra.find (\selectedUuid -> selectedUuid == fromEntityUuid healthCenterId) selectedHealthCentersUuid
                                            |> isJust
                                in
                                viewHealthCenter language ( healthCenterId, healthCenter ) isSynced
                            )
                            (Dict.toList healthCenters)
                        )
                    ]

        RemoteData.Failure error ->
            Utils.WebData.viewError language error

        RemoteData.Loading ->
            spinner

        RemoteData.NotAsked ->
            emptyNode


viewHealthCenter : Language -> ( HealthCenterId, HealthCenter ) -> Bool -> Html Msg
viewHealthCenter language ( healthCenterId, healthCenter ) isSynced =
    let
        ( syncLabel, syncMsg ) =
            if isSynced then
                ( "Remove from Sync list", RevisionIdAuthorityRemove healthCenterId )

            else
                ( "Add to Sync list", RevisionIdAuthorityAdd healthCenterId )
    in
    li [ style "margin-bottom" "5px" ]
        [ text <| healthCenter.name
        , button
            [ onClick syncMsg
            , style "margin-left" "20px"
            ]
            [ text syncLabel ]
        ]


viewSyncStatus : Language -> ModelIndexedDb -> Model -> Html Msg
viewSyncStatus language db model =
    details
        [ property "open" (Json.Encode.bool False)
        , class "segment ui"
        ]
        [ summary [] [ text "Sync Status" ]
        , div [] [ text <| "Sync status: " ++ Debug.toString model.syncStatus ]
        , case model.syncStatus of
            SyncDownloadGeneral webData ->
                viewSyncDownloadGeneral language model webData

            SyncDownloadAuthority webData ->
                viewSyncDownloadAuthority language db model webData

            _ ->
                emptyNode
        ]
