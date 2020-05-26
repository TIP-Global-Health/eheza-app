module DataManager.Encoder exposing (encodeIndexDbQueryUploadGeneralResultRecord)

import Backend.Person.Encoder
import DataManager.Model
    exposing
        ( BackendGeneralEntity(..)
        , IndexDbQueryUploadGeneralResultRecord
        , UploadMethod(..)
        )
import Json.Encode exposing (Value, int, list, object, string)
import List.Extra


encodeIndexDbQueryUploadGeneralResultRecord : IndexDbQueryUploadGeneralResultRecord -> List ( String, Value )
encodeIndexDbQueryUploadGeneralResultRecord record =
    let
        replacePhotoWithFileId encodedList fileId =
            encodedList
                -- Remove existing photo key.
                |> List.filter (\( key, _ ) -> key /= "photo")
                |> (\list_ -> ( "photo", int fileId ) :: list_)

        encodeData ( entity, method ) =
            (case entity of
                BackendGeneralPerson uuid _ entity_ ->
                    [ ( "uuid", string uuid )
                    , ( "type", string "person" )

                    -- @todo: Get correct file ID.
                    , ( "data", Json.Encode.object <| replacePhotoWithFileId (Backend.Person.Encoder.encodePerson entity_) 21053 )
                    , ( "method", encodeUploadMethod method )
                    ]

                _ ->
                    -- @todo
                    []
            )
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
