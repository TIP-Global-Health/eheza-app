module Gizra.String exposing
    ( isBlank
    , startsWithOneOf, endsWithOneOf, replacePrefixWith, requireAndStripPrefix
    , addLeadingZero, addLeadingZeroes
    )

{-| Some functions that work with strings.


## Whitespace

@docs isBlank


## Prefixes and suffixes

@docs startsWithOneOf, endsWithOneOf, replacePrefixWith, requireAndStripPrefix


## Padding

@docs addLeadingZero, addLeadingZeroes

-}

import Maybe.Extra exposing (isJust)
import Regex
import String exposing (dropLeft, endsWith, length, padLeft, startsWith)


{-| Checks whether the string starts with one of the provided prefixes.
If so, strips the prefix and returns the result. If not, returns `Nothing`.
Checks the prefixes in order, and uses the first which matches.

    "https://www.youtube.com/watch?v=abcdefghijklmnop"
        |> requireAndStripPrefix
            [ "https://www.youtube.com/watch?v="]
    --> Just "abcdefghijklmnop"

    "https://www.youtube.com/watch?v=abcdefghijklmnop"
        |> requireAndStripPrefix
            [ "https://www.youtube.com/watch?v="
            , "https://youtu.be/"
            ]
    --> Just "abcdefghijklmnop"

    "https://www.youtube.com/watch?v=abcdefghijklmnop"
        |> requireAndStripPrefix
            [ "https://youtu.be/"
            , "https://www.youtube.com/watch?v="
            ]
    --> Just "abcdefghijklmnop"

    "https://www.youtube.com/watch?v=abcdefghijklmnop"
        |> requireAndStripPrefix []
    --> Nothing

    "https://www.youtube.com/watch?v=abcdefghijklmnop"
        |> requireAndStripPrefix
            [ "https://apple.com/"
            , "https://microsoft.com"
            ]
    --> Nothing

-}
requireAndStripPrefix : List String -> String -> Maybe String
requireAndStripPrefix prefixes string =
    case startsWithOneOf prefixes string of
        Just prefix ->
            dropLeft (length prefix) string |> Just

        _ ->
            Nothing


{-| Checks whether the string starts with one of the provided prefixes.
If so, returns the prefix.

    startsWithOneOf [ "apple", "banana" ] "apple 27" --> Just "apple"

    startsWithOneOf [ "apple", "banana" ] "banana 27" --> Just "banana"

    startsWithOneOf [ "apple", "banana" ] "grapefruit 35" --> Nothing

-}
startsWithOneOf : List String -> String -> Maybe String
startsWithOneOf =
    validate startsWith


{-| Checks whether the string ends with one of the provided prefixes.
If so, returns the suffix.

    endsWithOneOf [ "apples", "bananas" ] "27 apples" --> Just "apples"

    endsWithOneOf [ "apples", "bananas" ] "27 bananas" --> Just "bananas"

    endsWithOneOf [ "apples", "bananas" ] "35 grapefruits" --> Nothing

-}
endsWithOneOf : List String -> String -> Maybe String
endsWithOneOf =
    validate endsWith


{-| If the string (third parameter) starts with the prefix (first parameter),
replace the prefix with a new prefix (second parameter). Otherwise, just
return the string.

    replacePrefixWith "http" "https" "http://apple.com/" --> "https://apple.com/"

    replacePrefixWith "http" "https" "ftp://ftp.apple.com/" --> "ftp://ftp.apple.com/"

-}
replacePrefixWith : String -> String -> String -> String
replacePrefixWith prefix newPrefix string =
    if startsWith prefix string then
        newPrefix ++ dropLeft (length prefix) string

    else
        string


validate : (String -> String -> Bool) -> List String -> String -> Maybe String
validate function options string =
    case options of
        option :: rest ->
            if function option string then
                Just option

            else
                -- Recursively check the rest of the suffixes.
                -- This should be tail-call optimized by the compiler,
                -- so we shouldn't need to worry about the stack.
                validate function rest string

        [] ->
            Nothing


{-| Pad the string to the desired length by adding leading zeroes.

    addLeadingZeroes 2 "1" --> "01"

    addLeadingZeroes 3 "17" --> "017"

    addLeadingZeroes 2 "27" --> "27"

-}
addLeadingZeroes : Int -> String -> String
addLeadingZeroes desiredLength =
    padLeft desiredLength '0'


{-| Add a leading zero to ensure that the string length is 2.

    addLeadingZero "1" --> "01"

    addLeadingZero "17" --> "17"

-}
addLeadingZero : String -> String
addLeadingZero =
    addLeadingZeroes 2


{-| Returns `True` if the string is empty, or composed only of whitespace.

    isBlank "" --> True

    isBlank "   " --> True

    isBlank "\t\n\u{000D}" --> True

    isBlank "blank" --> False

-}
isBlank : String -> Bool
isBlank string =
    case Regex.fromString "^\\s*$" of
        Just regex ->
            Regex.contains regex string

        Nothing ->
            -- This shouldn't occur, but we can't provie it to the compiler
            False
