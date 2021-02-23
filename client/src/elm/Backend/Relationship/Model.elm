module Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, RelatedBy(..), Relationship)

import Backend.Entities exposing (..)


{-| This reflects the structure as stored in the database.
-}
type alias Relationship =
    { person : PersonId
    , relatedTo : PersonId
    , relatedBy : RelatedBy
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


{-| These are the only values we store in the database. For `ChildOf`, we
would reverse `person` and `relatedTo` and put in the appropriate `relatedBy`.
-}
type RelatedBy
    = ParentOf
    | CaregiverFor


{-| This expresses all possible relations from a particular point of view --
that is, from the point of view of either person in the relationship.
-}
type MyRelatedBy
    = MyChild
    | MyParent
    | MyCaregiven
    | MyCaregiver


{-| This represents a relationship from the point of view of a specific person.
-}
type alias MyRelationship =
    { relatedTo : PersonId
    , relatedBy : MyRelatedBy
    }
