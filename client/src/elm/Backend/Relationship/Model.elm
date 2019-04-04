module Backend.Relationship.Model exposing (MyRelationship(..), RelatedBy(..), Relationship)

import Backend.Entities exposing (..)


{-| This reflects the structure as stored in the database.
-}
type alias Relationship =
    { person : PersonId
    , relatedTo : PersonId
    , relatedBy : RelatedBy
    }


{-| These are the only values we store in the database. For `ChildOf`, we
would reverse `person` and `relatedTo` and put in the appropriate `relatedBy`.
-}
type RelatedBy
    = ParentOf
    | CaregiverFor


{-| This represents a relationship from the point of view of a specific person.
-}
type MyRelationship
    = MyChild PersonId
    | MyParent PersonId
    | MyCaregiverFor PersonId
    | MyCaregiver PersonId
