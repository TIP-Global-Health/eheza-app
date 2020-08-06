module Backend.Relationship.Encoder exposing (encodePersonField, encodeRelatedBy, encodeRelatedByField, encodeRelatedToField, encodeRelationship, encodeRelationshipChanges)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeRelationship : Relationship -> List ( String, Value )
encodeRelationship data =
    [ encodePersonField data.person
    , encodeRelatedToField data.relatedTo
    , encodeRelatedByField data.relatedBy
    , ( "shard", maybe encodeEntityUuid data.shard )
    , ( "type", string "relationship" )
    ]


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
