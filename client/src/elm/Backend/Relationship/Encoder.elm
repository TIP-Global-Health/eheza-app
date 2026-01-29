module Backend.Relationship.Encoder exposing (encodeRelationship, encodeRelationshipChanges)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (RelatedBy(..), Relationship)
import Json.Encode exposing (Value, bool, string)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


encodeRelationship : Relationship -> List ( String, Value )
encodeRelationship data =
    [ encodePersonField data.person
    , encodeRelatedToField data.relatedTo
    , encodeRelatedByField data.relatedBy
    , ( "deleted", bool data.deleted )
    , ( "type", string "relationship" )
    ]
        ++ encodeIfSet "shard" data.shard encodeEntityUuid


encodeRelationshipChanges : { old : Relationship, new : Relationship } -> List ( String, Value )
encodeRelationshipChanges { old, new } =
    if old.person == new.person && old.relatedTo == new.relatedTo && old.relatedBy == new.relatedBy then
        []

    else
        encodeRelationship new


encodeRelatedBy : RelatedBy -> Value
encodeRelatedBy data =
    case data of
        ParentOf ->
            string "parent"

        CaregiverFor ->
            string "caregiver"


encodeRelatedByField : RelatedBy -> ( String, Value )
encodeRelatedByField data =
    ( "related_by", encodeRelatedBy data )


encodePersonField : PersonId -> ( String, Value )
encodePersonField data =
    ( "person", encodeEntityUuid data )


encodeRelatedToField : PersonId -> ( String, Value )
encodeRelatedToField data =
    ( "related_to", encodeEntityUuid data )
