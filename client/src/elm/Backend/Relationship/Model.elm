module Backend.Relationship.Model exposing (RelatedBy(..), Relationship)

import Backend.Entities exposing (..)


type alias Relationship =
    { person : PersonId
    , relatedTo : PersonId
    , relatedBy : RelatedBy
    }


type RelatedBy
    = ParentOf
    | CaregiverFor
