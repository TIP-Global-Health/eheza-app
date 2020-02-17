module Gizra.Html exposing
    ( keyed, divKeyed, keyedDivKeyed
    , showIf, showMaybe, emptyNode
    , intToPx, floatToPx
    , voidHref
    )

{-| Utilities for working with `Html`


## Keyed

@docs keyed, divKeyed, keyedDivKeyed


## Conditionally show HTML

@docs showIf, showMaybe, emptyNode


## CSS pixels

@docs intToPx, floatToPx


## Atrributes

@docs voidHref

-}

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Keyed
import Round


{-| A convenience for keyed divs.

You can use it just like `Html.div`.

    divKeyed []
        [ ( "first-key", functionThatProducesSomeHtml )
        , ( "second-key", anotherFunction )
        ]

-}
divKeyed : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
divKeyed =
    Html.Keyed.node "div"


{-| Like `divKeyed`, but you also provide a key for the div itself.
-}
keyedDivKeyed : String -> List (Attribute msg) -> List ( String, Html msg ) -> ( String, Html msg )
keyedDivKeyed key attrs children =
    keyed key <|
        divKeyed attrs children


{-| A convenience for putting things in keyed elements. It's just the
`(,)` operator, but it reads nicely in typical idioms. For instance,
you can do things like:

    keyedDiv []
        [ functionThatProducesHtml
            |> keyed "first-key"
        , anotherFunctionThatProducesHtml
            |> Html.map SomeMsgWrapper
            |> keyed "second-key"
        ]

Part of the reasoning here is that it's really the caller's job to provide a
key, since the key needs to be unique amongst the things in the list that the
caller is creating. So, functions that produce HTML shouldn't supply a key
themselves -- it should be supplied by the caller. And this is a convenient
idiom for doing that.

-}
keyed : String -> Html msg -> ( String, Html msg )
keyed =
    \a b -> ( a, b )


{-| Convert integer to CSS px.

    intToPx 27 --> "27px"

-}
intToPx : Int -> String
intToPx val =
    String.fromInt val ++ "px"


{-| Convert float to CSS px, with just 1 decimal point, since a px
is already pretty small.

    floatToPx 27 --> "27.0px"

    floatToPx 32.56 --> "32.6px"

-}
floatToPx : Float -> String
floatToPx val =
    Round.round 1 val ++ "px"


{-| Produces an empty text node for the DOM.
-}
emptyNode : Html msg
emptyNode =
    text ""


{-| Conditionally show Html. A bit cleaner than using if expressions in middle
of an html block:

    text "I'm shown"
        |> showIf True

    text "I'm not shown"
        |> showIf False

-}
showIf : Bool -> Html msg -> Html msg
showIf condition html =
    if condition then
        html

    else
        emptyNode


{-| Show Maybe Html if Just, or empty node if Nothing.
-}
showMaybe : Maybe (Html msg) -> Html msg
showMaybe =
    Maybe.withDefault emptyNode


{-| Sometimes you want to provide an `href` attribute in order to trigger
some browser behaviour, but you don't want a click to actually do anything.
You can use this attribute for that purpose -- it provides an

    href "javascript: void(0)"

... which does nothing when clicked, not even as much as `href "#"` would do.

-}
voidHref : Attribute msg
voidHref =
    href "javascript: void(0)"
