module Backend.Relationship.Decoder exposing (decodeRelatedBy, decodeRelationship)

import Backend.Relationship.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeRelationship : Decoder Relationship
decodeRelationship =
    decode Relationship
        |> required "person" decodeEntityUuid
        |> required "related_to" decodeEntityUuid
        |> required "related_by" decodeRelatedBy


decodeRelatedBy : Decoder RelatedBy
decodeRelatedBy =
    andThen
        (\s ->
            case s of
                "parent" ->
                    succeed ParentOf

                "caregiver" ->
                    succeed CaregiverFor

                _ ->
                    fail <|
                        s
                            ++ " is not a recognized RelatedBy"
        )
        string
