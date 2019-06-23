module Gizra.Maybe exposing (when)

{-| A module with some useful `Maybe` utilities.

@docs when

-}


{-| Create a `Just a` if condition is `True`, otherwise `Nothing`

    "something" |> when True --> Just "something"

    "something" |> when False --> Nothing

-}
when : Bool -> a -> Maybe a
when condition value =
    if condition then
        Just value

    else
        Nothing
