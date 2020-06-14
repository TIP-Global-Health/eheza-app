module DataManager.Encoder exposing
    ( encodeDataForDeferredPhotos
    , encodeIndexDbQueryUploadAuthorityResultRecord
    , encodeIndexDbQueryUploadGeneralResultRecord
    )

import DataManager.Model exposing (BackendAuthorityEntity(..), BackendEntityIdentifier, BackendGeneralEntity(..), IndexDbQueryUploadAuthorityResultRecord, IndexDbQueryUploadGeneralResultRecord, UploadMethod(..))
import DataManager.Utils
import Json.Encode exposing (Value, int, list, object, string)


encodeIndexDbQueryUploadGeneralResultRecord : IndexDbQueryUploadGeneralResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadGeneralResultRecord record =
    let
        encodeData ( entity, method ) =
            let
                identifier =
                    DataManager.Utils.getBackendGeneralEntityIdentifier entity
            in
            [ ( "uuid", string identifier.uuid )
            , ( "type", string identifier.type_ )
            , ( "method", encodeUploadMethod method )
            , ( "data", DataManager.Utils.encodeBackendGeneralEntity entity )
            ]
                |> object
    in
    [ ( "changes", list encodeData record.entities ) ]


encodeIndexDbQueryUploadAuthorityResultRecord : IndexDbQueryUploadAuthorityResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadAuthorityResultRecord record =
    let
        --replacePhotoWithFileId encodedEntity localId =
        --    let
        --        maybeFileId =
        --            Dict.get localId record.uploadPhotos
        --                |> Maybe.map (\row -> maybe int row.fileId)
        --                |> Maybe.withDefault null
        --    in
        --    encodedEntity
        --        -- Remove existing photo key.
        --        |> Dict.fromList
        --        -- Replace with file ID.
        --        |> Dict.insert "photo" maybeFileId
        --        |> Dict.toList
        encodeData ( entity, method ) =
            let
                identifier =
                    DataManager.Utils.getBackendAuthorityEntityIdentifier entity

                --data =
                --    case entity of
                --        BackendAuthorityPerson identifier_ ->
                --            let
                --                encodedEntity =
                --                    Backend.Person.Encoder.encodePerson identifier_.entity
                --
                --                encodedEntityUpdated =
                --                    if isJust identifier_.entity.avatarUrl then
                --                        replacePhotoWithFileId encodedEntity identifier_.revision
                --
                --                    else
                --                        encodedEntity
                --            in
                --            Json.Encode.object encodedEntityUpdated
                --
                --        _ ->
                --            -- @todo, get all the rest of entities that can have
                --            -- photos.
                --            Json.Encode.object []
            in
            [ ( "uuid", string identifier.uuid )
            , ( "type", string identifier.type_ )
            , ( "method", encodeUploadMethod method )
            , ( "data", DataManager.Utils.encodeBackendAuthorityEntity entity )
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
