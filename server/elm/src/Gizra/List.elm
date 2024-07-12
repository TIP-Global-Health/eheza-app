module Gizra.List exposing (insertAt, sortDescending)

{-| Some utility functions for `List`.

@docs insertAt, sortDescending

-}


{-| Insert an element at the specified position. Will insert at the
head if the position is negative, and at the end if the position is
out of range.

    insertAt -1 "d" [ "a", "b", "c" ] --> ["d", "a", "b", "c"]

    insertAt 0 "d" [ "a", "b", "c" ] --> ["d", "a", "b", "c"]

    insertAt 1 "d" [ "a", "b", "c" ] --> ["a", "d", "b", "c"]

    insertAt 100 "d" [ "a", "b", "c" ] --> ["a", "b", "c", "d"]

-}
insertAt : Int -> a -> List a -> List a
insertAt index element list =
    if index <= 0 then
        element :: list

    else
        List.take index list ++ (element :: List.drop index list)


{-| Sort the list in descending order.

    sortDescending [ "a", "c", "b" ] --> ["c", "b", "a"]

-}
sortDescending : List comparable -> List comparable
sortDescending =
    List.sortWith <|
        \v1 v2 ->
            case compare v1 v2 of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT
