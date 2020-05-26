module DataManager.Encoder exposing (encodeIndexDbQueryUploadGeneralResultRecord)

import Backend.Person.Encoder
import DataManager.Model
    exposing
        ( BackendGeneralEntity(..)
        , IndexDbQueryUploadGeneralResultRecord
        , UploadMethod(..)
        )
import DataManager.Utils exposing (getBackendGeneralEntityIdentifier)
import Json.Encode exposing (Value, int, list, object, string)


encodeIndexDbQueryUploadGeneralResultRecord : IndexDbQueryUploadGeneralResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadGeneralResultRecord record =
    let
        replacePhotoWithFileId encodedList fileId =
            encodedList
                -- Remove existing photo key.
                |> List.filter (\( key, _ ) -> key /= "photo")
                |> (\list_ -> ( "photo", int fileId ) :: list_)

        encodeData ( entity, method ) =
            let
                identifier =
                    getBackendGeneralEntityIdentifier entity

                data =
                    case entity of
                        BackendGeneralPerson uuid _ entity_ ->
                            -- @todo: Get correct file ID.
                            Json.Encode.object <| replacePhotoWithFileId (Backend.Person.Encoder.encodePerson entity_) 21053

                        _ ->
                            -- @todo
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
