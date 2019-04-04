module Backend.Relationship.Utils exposing (getRelatedTo, toMyRelationship)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (..)


{-| Consier a `Relationship` from the point of view of the specified person.
-}
toMyRelationship : PersonId -> Relationship -> Maybe MyRelationship
toMyRelationship id relationship =
    if relationship.person == id then
        case relationship.relatedBy of
            ParentOf ->
                Just <| MyChild relationship.relatedTo

            CaregiverFor ->
                Just <| MyCaregiverFor relationship.relatedTo

    else if relationship.relatedTo == id then
        case relationship.relatedBy of
            ParentOf ->
                Just <| MyParent relationship.person

            CaregiverFor ->
                Just <| MyCaregiver relationship.person

    else
        Nothing


getRelatedTo : MyRelationship -> PersonId
getRelatedTo myRelationship =
    case myRelationship of
        MyChild id ->
            id

        MyCaregiverFor id ->
            id

        MyParent id ->
            id

        MyCaregiver id ->
            id
