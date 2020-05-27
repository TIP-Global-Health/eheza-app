module Backend.Relationship.Encoder exposing (encodePersonField, encodeRelatedBy, encodeRelatedByField, encodeRelatedToField, encodeRelationship, encodeRelationshipChanges)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeRelationship : Relationship -> Value
encodeRelationship data =
    object
        [ encodePersonField data.person
        , encodeRelatedToField data.relatedTo
        , encodeRelatedByField data.relatedBy
        , ( "shard", maybe encodeEntityUuid data.shard )
        ]


encodeRelationshipChanges : { old : Relationship, new : Relationship } -> List ( String, Value )
encodeRelationshipChanges { old, new } =
    let
        person =
            if old.person == new.person then
                Nothing

            else
                Just <| encodePersonField new.person

        relatedTo =
            if old.relatedTo == new.relatedTo then
                Nothing

            else
                Just <| encodeRelatedToField new.relatedTo

        relatedBy =
            if old.relatedBy == new.relatedBy then
                Nothing

            else
                Just <| encodeRelatedByField new.relatedBy
    in
    [ person
    , relatedTo
    , relatedBy
    , Nothing
    ]
        |> List.filterMap identity


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
