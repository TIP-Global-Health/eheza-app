module Backend.Relationship.Utils exposing (isChildRelation, toMyRelationship, toRelationship)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, RelatedBy(..), Relationship)


{-| From the reference person's point of view, is the related person a child
they are responsible for - either their own child (`MyChild`) or one they are a
caregiver for (`MyCaregiven`)?

Used both to decide which related persons to fetch and which to assemble as
family members for a Family Nutrition encounter, so the two stay in step.

-}
isChildRelation : MyRelatedBy -> Bool
isChildRelation relatedBy =
    case relatedBy of
        MyChild ->
            True

        MyCaregiven ->
            True

        MyParent ->
            False

        MyCaregiver ->
            False


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
