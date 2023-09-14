module ZScore.Model exposing (..)

import RemoteData exposing (RemoteData(..), WebData)
import Utils.AllDict exposing (AllDict)


{-| This represents the data that we use to calculate ZScores.
We load this data from a cache, so we provide some operations here to
do that.

So, basically you need to issue a `FetchData` at some point (e.g. at
application startup), and then provide the `Model` to the functions in
`Utils`.

-}
type alias Model =
    { bmiForAge : WebData BmiForAgeTables
    , lengthHeightForAge : WebData LengthHeightForAgeTables
    , weightForAge : WebData WeightForAgeTables
    , weightForHeight : WebData WeightForHeightTables
    , weightForLength : WebData WeightForLengthTables
    , headCircumferenceForAge : WebData HeadCircumferenceForAgeTables
    }


emptyModel : Model
emptyModel =
    { bmiForAge = NotAsked
    , lengthHeightForAge = NotAsked
    , weightForAge = NotAsked
    , weightForHeight = NotAsked
    , weightForLength = NotAsked
    , headCircumferenceForAge = NotAsked
    }


{-| So, we're fetching the underlying data via HTTP. But, we're caching it
locally, so (a) it will be fast, and (b) it will work offline.
-}
type Msg
    = FetchAllTables
    | FetchBmiForAgeTables
    | FetchLengthHeightForAgeTables
    | FetchWeightForAgeTables
    | FetchWeightForHeightTables
    | FetchWeightForLengthTables
    | FetchHeadCircumferenceForAgeTables
    | HandleBmiForAgeTables (WebData BmiForAgeTables)
    | HandleLengthHeightForAgeTables (WebData LengthHeightForAgeTables)
    | HandleWeightForAgeTables (WebData WeightForAgeTables)
    | HandleWeightForHeightTables (WebData WeightForHeightTables)
    | HandleWeightForLengthTables (WebData WeightForLengthTables)
    | HandleHeadCircumferenceForAgeTables (WebData HeadCircumferenceForAgeTables)


{-| For now, just make ZScore an alias for a Float ... we could do fancier
things here, if it seems helpful. For instance, we could have phantom
types differentiating between weight-for-length, weight-for-age, etc. But that
seems like it might be overkill.
-}
type alias ZScore =
    Float



{- We might want to re-use some of these types generally. In fact, there would
   be ways to be more sophisticated about measurement units, but they might be
   overkill.

   The types are mainly to avoid any confusion about what the units are. It
   forces the caller to do things like:

       zScoreFromHeight (AgeDay 27) Male (Length 27)

   ... so that the caller has to think about what units are being provided.
-}


{-| A wrapper for an integer representing days.
-}
type Days
    = Days Int


{-| A wrapper for an integer representing months.
-}
type Months
    = Months Int


type Length
    = Length Float


type Height
    = Height Float


{-| In some cases, we accept a `length` or a `height`.
-}
type Centimetres
    = Centimetres Float


type Kilograms
    = Kilograms Float


type BMI
    = BMI Float


type ChartAgeRange
    = RangeBirthToThirteenWeeks
    | RangeBirthToTwoYears
    | RangeBirthToFiveYears
    | RangeFiveToTenYears
    | RangeFiveToNineteenYears


type alias ByDaysAndMonths value =
    { byDay : AllDict Days (ZScoreEntry value) Int
    , byMonth : AllDict Months (ZScoreEntry value) Int
    }


type alias BmiForAgeTables =
    MaleAndFemale (ByDaysAndMonths BMI)


{-| These tables represent length for children < 2 yrs old,
and height for >= 2 years old.
-}
type alias LengthHeightForAgeTables =
    MaleAndFemale (ByDaysAndMonths Centimetres)


type alias WeightForAgeTables =
    MaleAndFemale (ByDaysAndMonths Kilograms)


type alias WeightForLengthTables =
    MaleAndFemale (AllDict Length (ZScoreEntry Kilograms) Int)


type alias WeightForHeightTables =
    MaleAndFemale (AllDict Height (ZScoreEntry Kilograms) Int)


type alias HeadCircumferenceForAgeTables =
    MaleAndFemale (AllDict Days (ZScoreEntry Centimetres) Int)


type alias MaleAndFemale a =
    { male : a
    , female : a
    }


type alias ZScoreEntry value =
    { l : Float
    , m : value
    , s : Float
    }
