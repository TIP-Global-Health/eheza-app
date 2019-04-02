module Backend.Relationship.Encoder exposing (encodeRelatedBy, encodeRelationship)

import Backend.Relationship.Model exposing (..)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeRelationship : Relationship -> Value
encodeRelationship data =
    object
        [ ( "person", encodeEntityUuid data.person )
        , ( "related_to", encodeEntityUuid data.relatedTo )
        , ( "related_by", encodeRelatedBy data.relatedBy )
        ]


encodeRelatedBy : RelatedBy -> Value
encodeRelatedBy data =
    case data of
        ParentOf ->
            string "parent"

        CaregiverFor ->
            string "caregiver"
