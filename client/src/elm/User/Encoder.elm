module User.Encoder exposing (encodeUser)

import EverySet exposing (EverySet)
import Json.Encode exposing (Value, int, list, object, string)
import Restful.Endpoint exposing (encodeEntityId)
import User.Model exposing (Role(..), User)


{-| This is mostly for caching the user in local storage.
It needs to be the inverse of `decodeUser`.
-}
encodeUser : User -> Value
encodeUser user =
    object
        [ ( "id", int user.id )
        , ( "label", string user.name )
        , ( "avatar_url", string user.avatarUrl )
        , ( "clinics", list (List.map encodeEntityId user.clinics) )
        , ( "roles", list (List.map encodeRole (EverySet.toList user.roles)) )
        ]


encodeRole : Role -> Value
encodeRole role =
    case role of
        Anonymous ->
            string "anonymous user"

        Authenticated ->
            string "authenticated user"

        Administrator ->
            string "administrator"

        Nurse ->
            string "nurse"
