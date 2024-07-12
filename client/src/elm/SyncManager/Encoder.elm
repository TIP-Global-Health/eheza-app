module SyncManager.Encoder exposing
    ( encodeDataForDeferredPhotos
    , encodeDeviceStateReport
    , encodeIncidentDetails
    , encodeIndexDbQueryUploadAuthorityResultRecord
    , encodeIndexDbQueryUploadGeneralResultRecord
    , encodeIndexDbQueryUploadWhatsAppResultRecord
    , encodeSyncIncident
    )

import AssocList as Dict
import Backend.Measurement.Encoder
import Backend.Person.Encoder
import Backend.StockUpdate.Encoder
import Components.ReportToWhatsAppDialog.Encoder exposing (encodeReportType)
import Gizra.NominalDate
import Json.Encode exposing (Value, int, list, null, object, string)
import Json.Encode.Extra exposing (maybe)
import Maybe.Extra exposing (isJust)
import SyncManager.Model
    exposing
        ( BackendAuthorityEntity(..)
        , BackendEntityIdentifier
        , BackendWhatsAppEntity
        , IndexDbQueryUploadAuthorityResultRecord
        , IndexDbQueryUploadGeneralResultRecord
        , IndexDbQueryUploadWhatsAppResultRecord
        , SyncIncidentType(..)
        , UploadMethod(..)
        )
import SyncManager.Utils
import Translate.Utils exposing (encodeLanguage)


encodeIndexDbQueryUploadGeneralResultRecord : Int -> IndexDbQueryUploadGeneralResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadGeneralResultRecord dbVersion record =
    let
        encodeData ( entity, method ) =
            let
                identifier =
                    SyncManager.Utils.getBackendGeneralEntityIdentifier entity
            in
            [ ( "uuid", string identifier.uuid )
            , ( "type", string identifier.type_ )
            , ( "method", encodeUploadMethod method )
            , ( "data", SyncManager.Utils.encodeBackendGeneralEntity entity )
            ]
                |> object
    in
    [ ( "changes", list encodeData record.entities )
    , ( "db_version", string <| String.fromInt dbVersion )
    ]


encodeIndexDbQueryUploadWhatsAppResultRecord : Int -> IndexDbQueryUploadWhatsAppResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadWhatsAppResultRecord dbVersion record =
    let
        encodeData entity =
            [ ( "type", string "whatsapp_record" )
            , ( "method", encodeUploadMethod UploadMethodCreate )
            , ( "data", encodeBackendWhatsAppEntity entity )
            ]
                |> object
    in
    [ ( "changes", list encodeData record.entities )
    , ( "db_version", string <| String.fromInt dbVersion )
    ]


encodeBackendWhatsAppEntity : BackendWhatsAppEntity -> Value
encodeBackendWhatsAppEntity entity =
    [ ( "person", string entity.personId )
    , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD entity.dateMeasured )
    , ( "language", encodeLanguage entity.language )
    , ( "report_type", encodeReportType entity.reportType )
    , ( "phone_number", string entity.phoneNumber )
    , ( "screenshot", int entity.screenshot )
    ]
        |> Json.Encode.object


encodeIndexDbQueryUploadAuthorityResultRecord : Int -> IndexDbQueryUploadAuthorityResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadAuthorityResultRecord dbVersion record =
    let
        replacePhotoWithFileId localId imageField encodedEntity =
            let
                maybeFileId =
                    Dict.get localId record.uploadPhotos
                        |> Maybe.map (\row -> maybe int row.fileId)
                        |> Maybe.withDefault null
            in
            encodedEntity
                -- Remove existing photo key.
                |> Dict.fromList
                -- Replace with file ID.
                |> Dict.insert imageField maybeFileId
                |> Dict.toList

        encodeData ( entity, method ) =
            let
                identifier =
                    SyncManager.Utils.getBackendAuthorityEntityIdentifier entity

                doEncode encoder identifier_ imageField =
                    encoder identifier_.entity
                        |> replacePhotoWithFileId identifier_.revision imageField
                        |> List.append [ ( "uuid", string identifier_.uuid ) ]
                        |> Json.Encode.object

                data =
                    case entity of
                        BackendAuthorityPerson identifier_ ->
                            let
                                encodedEntity =
                                    Backend.Person.Encoder.encodePerson identifier_.entity

                                encodedEntityUpdated =
                                    if isJust identifier_.entity.avatarUrl then
                                        replacePhotoWithFileId identifier_.revision "photo" encodedEntity

                                    else
                                        encodedEntity
                            in
                            encodedEntityUpdated
                                |> List.append [ ( "uuid", string identifier_.uuid ) ]
                                |> Json.Encode.object

                        BackendAuthorityPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodePhoto
                                identifier_
                                "photo"

                        BackendAuthorityNutritionPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodeNutritionPhoto
                                identifier_
                                "photo"

                        BackendAuthorityPrenatalPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodePrenatalPhoto
                                identifier_
                                "photo"

                        BackendAuthorityWellChildPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodeWellChildPhoto
                                identifier_
                                "photo"

                        BackendAuthorityStockUpdate identifier_ ->
                            doEncode
                                Backend.StockUpdate.Encoder.encodeStockUpdate
                                identifier_
                                "signature"

                        _ ->
                            SyncManager.Utils.encodeBackendAuthorityEntity entity
            in
            [ ( "uuid", string identifier.uuid )
            , ( "type", string identifier.type_ )
            , ( "method", encodeUploadMethod method )
            , ( "data", data )
            ]
                |> object
    in
    [ ( "changes", list encodeData record.entities )
    , ( "db_version", string <| String.fromInt dbVersion )
    ]


encodeDataForDeferredPhotos : String -> BackendEntityIdentifier -> String
encodeDataForDeferredPhotos photoUrl entityIdentifier =
    [ ( "uuid", string entityIdentifier.uuid )

    -- We place the `photo` and `attempts` under `entity`, as this is what
    -- elmApp.ports.sendSyncedDataToIndexDb is expecting.
    , ( "entity"
      , object
            [ ( "photo", string photoUrl )
            , ( "attempts", int 0 )
            ]
      )
    , ( "vid", int entityIdentifier.revision )
    ]
        |> Json.Encode.object
        |> Json.Encode.encode 0


encodeDeviceStateReport : String -> String -> Int -> List String -> List ( String, Value )
encodeDeviceStateReport version phase totalToUpload syncedAutorities =
    [ ( "version", string version )
    , ( "phase", string phase )
    , ( "total_to_upload", int totalToUpload )
    , ( "synced_authorities", list string syncedAutorities )
    ]


encodeSyncIncident : SyncIncidentType -> List ( String, Value )
encodeSyncIncident incidentType =
    case incidentType of
        FileUploadIncident identifier ->
            [ ( "incident_type", string "file-upload" )
            , ( "content_identifier", string identifier )
            ]

        ContentUploadIncident identifier ->
            [ ( "incident_type", string "content-upload" )
            , ( "content_identifier", string identifier )
            ]


encodeUploadMethod : UploadMethod -> Value
encodeUploadMethod uploadMethod =
    case uploadMethod of
        UploadMethodCreate ->
            string "POST"

        UploadMethodUpdate ->
            string "PATCH"


encodeIncidentDetails : String -> List ( String, Value )
encodeIncidentDetails details =
    [ ( "incident_details", string details ) ]
