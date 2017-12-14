module ZScore.Model
    exposing
        ( Centimetres(..)
        , Kilograms(..)
        , Model
        , Msg(..)
        , ZScore(..)
        , ZScoreEntry
        , emptyModel
        )

import IntDict exposing (IntDict)
import RemoteData exposing (WebData, RemoteData(..))


{-| This represents the data that we use to calculate ZScores.
We load this data from a cache, so we provide some operations here to
do that.

So, basically you need to issue a `FetchData` at some point (e.g. at
application startup), and then provide the `Model` to the functions in
`Utils`.

-}
type alias Model =
    { heightForAgeBoys : WebData (IntDict ZScoreEntry)
    , heightForAgeGirls : WebData (IntDict ZScoreEntry)
    , weightForAgeBoys : WebData (IntDict ZScoreEntry)
    , weightForAgeGirls : WebData (IntDict ZScoreEntry)
    , weightForHeightBoys : WebData (IntDict ZScoreEntry)
    , weightForHeightGirls : WebData (IntDict ZScoreEntry)
    }


emptyModel : Model
emptyModel =
    { heightForAgeBoys = NotAsked
    , heightForAgeGirls = NotAsked
    , weightForAgeBoys = NotAsked
    , weightForAgeGirls = NotAsked
    , weightForHeightBoys = NotAsked
    , weightForHeightGirls = NotAsked
    }


{-| So, we're fetching the underlying data via HTTP. But, we're caching it
locally, so (a) it will be fast, and (b) it will work offline.
-}
type Msg
    = FetchAll
    | FetchHeightForAgeBoys
    | FetchHeightForAgeGirls
    | FetchWeightForAgeBoys
    | FetchWeightForAgeGirls
    | FetchWeightForHeightBoys
    | FetchWeightForHeightGirls
    | HandleHeightForAgeBoys (WebData (IntDict ZScoreEntry))
    | HandleHeightForAgeGirls (WebData (IntDict ZScoreEntry))
    | HandleWeightForAgeBoys (WebData (IntDict ZScoreEntry))
    | HandleWeightForAgeGirls (WebData (IntDict ZScoreEntry))
    | HandleWeightForHeightBoys (WebData (IntDict ZScoreEntry))
    | HandleWeightForHeightGirls (WebData (IntDict ZScoreEntry))



{- We might want to re-use some of these types generally. In fact, there would
   be ways to be more sophisticated about measurement units, but they might be
   overkill.

   The types are mainly to avoid any confusion about what the units are. It
   forces the caller to do things like:

       zScoreFromHeight (AgeDay 27) Male (Centimetres 27)

   ... so that the caller has to think about what units are being provided.
-}


type Centimetres
    = Centimetres Float


type Kilograms
    = Kilograms Float


{-| A ZScore. This isn't really a number -- e.g. you can't
meaningfully do any arithmetic with it. So, we just enumerate the
possibilities.

  - Use `compareZScore` to check ordering.

  - You can use Elm's `==` reliably with this type.

  - Use `viewZScore` to get the ZScore as a string, such that
    `ZSCore3Neg` displays as "-3", etc.

Note that when we calculate a ZScore from a measurement, we apply a kind
of "ceiling" ... if a measurement is between two ZScore lines, we report
the higher one.

-}
type ZScore
    = ZScore4Neg
    | ZScore3Neg
    | ZScore2Neg
    | ZScore1Neg
    | ZScore0
    | ZScore1
    | ZScore2
    | ZScore3
    | ZScore4


type alias ZScoreEntry =
    { sd0 : Float
    , sd1 : Float
    , sd2 : Float
    , sd3 : Float
    , sd4 : Float
    , sd1neg : Float
    , sd2neg : Float
    , sd3neg : Float
    , sd4neg : Float
    }
