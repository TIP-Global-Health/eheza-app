module Backend.Entities exposing (..)

{-| -}

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| This is a wrapper for an `Int` id. It takes a "phantom" type variable
in order to gain type-safety about what kind of entity it is an ID for.
So, to specify that you have an id for a clinic, you would say:

    type ClinicId
        = ClinicId

    clinidId : EntityId ClinicId

-}
type EntityId a
    = EntityId Int


{-| This is how you create an `EntityId`, if you have an `Int`. You can create
any kind of `EntityId` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed `EntityIds`.
-}
toEntityId : Int -> EntityId a
toEntityId =
    EntityId


{-| This is how you get an `Int` back from a `EntityId`. You should only use
this in boundary situations, where you need to send the id out in an untyped
way. Normally, you should just pass around the `EntityId` itself, to retain
type-safety.
-}
fromEntityId : EntityId a -> Int
fromEntityId (EntityId a) =
    a


{-| Decodes a EntityId.

This just turns JSON int (or string that is an int) to an `EntityId`. You need
to supply the `field "id"` yourself, if necessary, since id's could be present
in other fields as well.

This decodes any kind of `EntityId` you like (since there is fundamentally no type
information in the JSON iself, of course). So, you need to verify that the type
is correct yourself.

-}
decodeEntityId : Decoder (EntityId a)
decodeEntityId =
    Json.Decode.map toEntityId Json.Decode.int


{-| Encodes any kind of `EntityId` as a JSON int.
-}
encodeEntityId : EntityId a -> Value
encodeEntityId =
    Json.Encode.int << fromEntityId


{-| This is a wrapper for an UUID.
-}
type EntityUuid a
    = EntityUuid String


{-| This is how you create a `EntityUuid`, if you have a `String`. You can create
any kind of `EntityUuid` this way ... so you would normally only do this in
situations that are fundamentally untyped, such as when you are decoding
JSON data. Except in those kind of "boundary" situations, you should be
working with the typed `EntityUuid`s.
-}
toEntityUuid : String -> EntityUuid a
toEntityUuid =
    EntityUuid


{-| This is how you get a `String` back from an `EntityUuid`. You should only use
this in boundary situations, where you need to send the UUID out in an untyped
way. Normally, you should just pass around the `EntityUuid` itself, to retain
type-safety.
-}
fromEntityUuid : EntityUuid a -> String
fromEntityUuid (EntityUuid a) =
    a


{-| Decodes an `EntityUuid`.
-}
decodeEntityUuid : Decoder (EntityUuid a)
decodeEntityUuid =
    Json.Decode.map toEntityUuid Json.Decode.string


{-| Encodes any kind of `EntityUuid` as a JSON string.
-}
encodeEntityUuid : EntityUuid a -> Value
encodeEntityUuid =
    Json.Encode.string << fromEntityUuid
