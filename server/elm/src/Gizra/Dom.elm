module Gizra.Dom exposing (Rectangle, currentTarget, target, findAncestor, checkId, decodeDomRect)

{-| Some utility functions for accessing the DOM.

@docs Rectangle, currentTarget, target, findAncestor, checkId, decodeDomRect

-}

import Json.Decode exposing (Decoder, at, decodeValue, fail, field, float, lazy, map, map3, map4, oneOf, string, succeed)


{-| Applies the supplied decoder to the "target" field.

    import Json.Decode exposing (..)
    import Result exposing (Result(..), mapError)

    """
        { "target":
            { "id": "an-id" }
        }
    """
        |> decodeString (target (field "id" string))
    --> Ok "an-id"

-}
target : Decoder a -> Decoder a
target =
    field "target"


{-| Like `target`, but instead of getting the element that received the
event, this gets the element on which the event handler was placed.

    """
        { "currentTarget":
            { "id": "an-id" }
        }
    """
        |> decodeString (currentTarget (field "id" string))
    --> Ok "an-id"

-}
currentTarget : Decoder a -> Decoder a
currentTarget =
    field "currentTarget"


{-| You supply two decoders.

We use the first decoder to select an ancestor. We start with the object
itself, and then look at each of its ancestors in turn (via `parentElement`).
We stop when the first decoder succeeds with a `True`.

Then, we take the selected ancestor and apply the second decoder

The resulting decoder will fail if no ancestor is found via the first decoder,
or if the second decoder fails once we've found an ancestor to apply it to.

    """
        { "id": "id1"
        , "height": 1
        }
    """
        |> decodeString (findAncestor (checkId "id1") (field "height" int))
    --> Ok 1

    """
        { "id": "id1"
        , "height": 1
        }
    """
        |> decodeString (findAncestor (checkId "id1") (field "non-existent" int))
        |> mapError (always "")
    --> Err ""

    """
        { "id": "id1"
        , "height": 1
        }
    """
        |> decodeString (findAncestor (checkId "non-existent") (field "height" int))
        |> mapError (always "")
    --> Err ""

    """
        { "id": "id1"
        , "height": 1
        , "parentElement":
            { "id": "id2"
            , "height": 2
            }
        }
    """
        |> decodeString (findAncestor (checkId "id2") (field "height" int))
    --> Ok 2

    """
        { "id": "id1"
        , "height": 1
        , "parentElement":
            { "id": "id2"
            , "height": 2
            , "parentElement":
                { "id": "id3"
                , "height": 3
                }
            }
        }
    """
        |> decodeString (findAncestor (checkId "id3") (field "height" int))
    --> Ok 3

-}
findAncestor : Decoder Bool -> Decoder a -> Decoder a
findAncestor finder decoder =
    let
        checkFinder =
            finder
                |> Json.Decode.andThen
                    (\found ->
                        if found then
                            decoder

                        else
                            fail "Keep trying"
                    )

        checkParent =
            field "parentElement" <|
                lazy (\_ -> findAncestor finder decoder)
    in
    oneOf
        [ checkFinder
        , checkParent
        ]


{-| The dimensions of a rectangle.
-}
type alias Rectangle =
    { top : Float
    , left : Float
    , width : Float
    , height : Float
    }


{-| Decodes a `DOMRect` Javascript object.

    """
        { "top": 27
        , "left": 32
        , "width": 45
        , "height": 72
        }
    """
        |> decodeString decodeDomRect
            --> Ok
            { top = 27
            , left = 32
            , width = 45
            , height = 72
            }

-}
decodeDomRect : Decoder Rectangle
decodeDomRect =
    map4
        (\top left width height ->
            { top = top
            , left = left
            , width = width
            , height = height
            }
        )
        (field "top" float)
        (field "left" float)
        (field "width" float)
        (field "height" float)


{-| Provides a decoder which indicates whether the `id` field is equal to the
provided id. Fails if there is no `id` field.

    """ { "id": "an-id" } """
        |> decodeString (checkId "an-id")
    --> Ok True

    """ { "id": "an-id" } """
        |> decodeString (checkId "a-different-id")
    --> Ok False

    """ { } """
        |> decodeString (checkId "an-id")
        |> mapError (always "")
    --> Err ""

-}
checkId : String -> Decoder Bool
checkId id =
    field "id" string
        |> map ((==) id)
