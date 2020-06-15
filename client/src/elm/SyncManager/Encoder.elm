module SyncManager.Encoder exposing
    ( encodeDataForDeferredPhotos
    , encodeIndexDbQueryUploadAuthorityResultRecord
    , encodeIndexDbQueryUploadGeneralResultRecord
    )

import AssocList as Dict
import Backend.Measurement.Encoder
import Backend.Person.Encoder
import Json.Encode exposing (Value, int, list, null, object, string)
import Json.Encode.Extra exposing (maybe)
import Maybe.Extra exposing (isJust)
import SyncManager.Model exposing (BackendAuthorityEntity(..), BackendEntityIdentifier, BackendGeneralEntity(..), IndexDbQueryUploadAuthorityResultRecord, IndexDbQueryUploadGeneralResultRecord, UploadMethod(..))
import SyncManager.Utils


encodeIndexDbQueryUploadGeneralResultRecord : IndexDbQueryUploadGeneralResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadGeneralResultRecord record =
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
    [ ( "changes", list encodeData record.entities ) ]


encodeIndexDbQueryUploadAuthorityResultRecord : IndexDbQueryUploadAuthorityResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadAuthorityResultRecord record =
    let
        replacePhotoWithFileId localId encodedEntity =
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
                |> Dict.insert "photo" maybeFileId
                |> Dict.toList

        encodeData ( entity, method ) =
            let
                identifier =
                    SyncManager.Utils.getBackendAuthorityEntityIdentifier entity

                doEncode encoder identifier_ =
                    encoder identifier_.entity
                        |> replacePhotoWithFileId identifier_.revision
                        |> Json.Encode.object

                data =
                    case entity of
                        BackendAuthorityNutritionPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodeNutritionPhoto
                                identifier_

                        BackendAuthorityPerson identifier_ ->
                            let
                                encodedEntity =
                                    Backend.Person.Encoder.encodePerson identifier_.entity

                                encodedEntityUpdated =
                                    if isJust identifier_.entity.avatarUrl then
                                        replacePhotoWithFileId identifier_.revision encodedEntity

                                    else
                                        encodedEntity
                            in
                            Json.Encode.object encodedEntityUpdated

                        BackendAuthorityPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodePhoto
                                identifier_

                        BackendAuthorityPrenatalPhoto identifier_ ->
                            doEncode
                                Backend.Measurement.Encoder.encodePrenatalPhoto
                                identifier_

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
    [ ( "changes", list encodeData record.entities ) ]


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


encodeUploadMethod : UploadMethod -> Value
encodeUploadMethod uploadMethod =
    case uploadMethod of
        UploadMethodCreate ->
            string "POST"

        UploadMethodUpdate ->
            string "PATCH"
