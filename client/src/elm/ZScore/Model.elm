module ZScore.Model
    exposing
        ( AgeInDays(..)
        , Centimetres(..)
        , Kilograms(..)
        , ZScore(..)
        )

{- We might want to re-use some of these types generally. In fact, there would
   be ways to be more sophisticated about measurement units, but they might be
   overkill.

   The types are mainly to avoid any confusion about what the units are. It
   forces the caller to do things like:

       zScoreFromHeight (AgeDay 27) Male (Centimetres 27)

   ... so that the caller has to think about what units are being provided.
-}


type AgeInDays
    = AgeInDays Int


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

-}
type ZScore
    = ZScore3Neg
    | ZScore2Neg
    | ZScore1Neg
    | ZScore0
    | ZScore1
    | ZScore2
    | ZScore3
