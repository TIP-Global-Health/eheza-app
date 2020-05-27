module DataManager.Encoder exposing (encodeIndexDbQueryUploadGeneralResultRecord)

import AssocList as Dict
import Backend.Person.Encoder
import DataManager.Model
    exposing
        ( BackendGeneralEntity(..)
        , IndexDbQueryUploadGeneralResultRecord
        , UploadMethod(..)
        )
import DataManager.Utils exposing (getBackendGeneralEntityIdentifier)
import Json.Encode exposing (Value, int, list, null, object, string)
import Json.Encode.Extra exposing (maybe)


encodeIndexDbQueryUploadGeneralResultRecord : IndexDbQueryUploadGeneralResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadGeneralResultRecord record =
    let
        replacePhotoWithFileId encodedEntity localId =
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
                    getBackendGeneralEntityIdentifier entity

                data =
                    case entity of
                        BackendGeneralPerson _ localId entity_ ->
                            let
                                encodedEntity =
                                    Backend.Person.Encoder.encodePerson entity_

                                encodedEntityUpdated =
                                    case entity_.avatarUrl of
                                        Just photo ->
                                            -- @todo: Get correct file ID.
                                            replacePhotoWithFileId encodedEntity localId

                                        Nothing ->
                                            encodedEntity
                            in
                            Json.Encode.object encodedEntityUpdated

                        _ ->
                            -- @todo, get all the rest.
                            Json.Encode.object []
            in
            [ ( "uuid", string identifier.uuid )
            , ( "type", string identifier.type_ )
            , ( "method", encodeUploadMethod method )
            , ( "data", data )
            ]
                |> object
    in
    [ ( "changes", list encodeData record.entities ) ]


encodeUploadMethod : UploadMethod -> Value
encodeUploadMethod uploadMethod =
    case uploadMethod of
        UploadMethodCreate ->
            string "POST"

        UploadMethodUpdate ->
            string "PATCH"
