module Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, RelatedBy(..), Relationship)


{-| Consider a `Relationship` from the point of view of the specified person.
-}
toMyRelationship : PersonId -> Relationship -> Maybe MyRelationship
toMyRelationship id relationship =
    if relationship.person == id then
        case relationship.relatedBy of
            ParentOf ->
                Just
                    { relatedTo = relationship.relatedTo
                    , relatedBy = MyChild
                    }

            CaregiverFor ->
                Just
                    { relatedTo = relationship.relatedTo
                    , relatedBy = MyCaregiven
                    }

    else if relationship.relatedTo == id then
        case relationship.relatedBy of
            ParentOf ->
                Just
                    { relatedTo = relationship.person
                    , relatedBy = MyParent
                    }

            CaregiverFor ->
                Just
                    { relatedTo = relationship.person
                    , relatedBy = MyCaregiver
                    }

    else
        Nothing


{-| Reverse the above ... that is, turn a `MyRelationship` back into the
normalized form we use in the database.
-}
toRelationship : PersonId -> MyRelationship -> Maybe HealthCenterId -> Relationship
toRelationship personId myRelationship shard =
    case myRelationship.relatedBy of
        MyParent ->
            { person = myRelationship.relatedTo
            , relatedTo = personId
            , relatedBy = ParentOf
            , deleted = False
            , shard = shard
            }

        MyChild ->
            { person = personId
            , relatedTo = myRelationship.relatedTo
            , relatedBy = ParentOf
            , deleted = False
            , shard = shard
            }

        MyCaregiver ->
            { person = myRelationship.relatedTo
            , relatedTo = personId
            , relatedBy = CaregiverFor
            , deleted = False
            , shard = shard
            }

        MyCaregiven ->
            { person = personId
            , relatedTo = myRelationship.relatedTo
            , relatedBy = CaregiverFor
            , deleted = False
            , shard = shard
            }
