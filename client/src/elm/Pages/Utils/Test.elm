module Pages.Utils.Test exposing (all)

import Backend.Measurement.Utils exposing (muacValueForSite)
import Expect
import Pages.Utils exposing (muacUnitTransIdForSite, percentageOfTotal, setMuacValueForSite)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Translate


muacValueForSiteTest : Test
muacValueForSiteTest =
    -- MUAC is stored in cm everywhere. Burundi enters and reads it in mm, so the
    -- two conversions have to agree: whatever is typed must come back unchanged.
    describe "MUAC conversions between what is stored and what is shown"
        [ test "Burundi: 220 typed is stored as 22 cm" <|
            \_ ->
                setMuacValueForSite SiteBurundi "220"
                    |> Expect.equal (Just 22)
        , test "Burundi: 22 cm is shown as 220" <|
            \_ ->
                muacValueForSite SiteBurundi 22
                    |> Expect.equal 220
        , test "Burundi: what is typed comes back the same" <|
            \_ ->
                setMuacValueForSite SiteBurundi "225"
                    |> Maybe.map (muacValueForSite SiteBurundi)
                    |> Expect.equal (Just 225)
        , test "Rwanda: the value is left alone in both directions" <|
            \_ ->
                ( setMuacValueForSite SiteRwanda "22.5"
                , muacValueForSite SiteRwanda 22.5
                )
                    |> Expect.equal ( Just 22.5, 22.5 )
        , test "Somalia and unknown sites are left alone too" <|
            \_ ->
                ( muacValueForSite SiteSomalia 22.5
                , muacValueForSite SiteUnknown 22.5
                )
                    |> Expect.equal ( 22.5, 22.5 )
        , test "a value that isn't a number gives nothing" <|
            \_ ->
                setMuacValueForSite SiteBurundi ""
                    |> Expect.equal Nothing
        ]


percentageOfTotalTest : Test
percentageOfTotalTest =
    describe "Pages.Utils.percentageOfTotal"
        [ test "returns 0 when the total is 0, instead of NaN" <|
            \_ ->
                percentageOfTotal 0 0
                    |> Expect.equal 0
        , test "returns 0 for a positive part over a zero total" <|
            \_ ->
                percentageOfTotal 5 0
                    |> Expect.equal 0
        , test "computes a rounded proportion" <|
            \_ ->
                percentageOfTotal 3 4
                    |> Expect.equal 75
        , test "rounds to the nearest whole percent" <|
            \_ ->
                percentageOfTotal 1 3
                    |> Expect.equal 33
        , test "is 100 when part equals total" <|
            \_ ->
                percentageOfTotal 7 7
                    |> Expect.equal 100
        ]


muacUnitTransIdForSiteTest : Test
muacUnitTransIdForSiteTest =
    -- The label has to say the same unit the value is entered and read in, or
    -- the number on screen means something other than what it says.
    describe "muacUnitTransIdForSite"
        [ test "Burundi reads MUAC in millimetres" <|
            \_ ->
                muacUnitTransIdForSite SiteBurundi
                    |> Expect.equal Translate.UnitMillimeter
        , test "everywhere else reads it in centimetres" <|
            \_ ->
                ( muacUnitTransIdForSite SiteRwanda
                , muacUnitTransIdForSite SiteSomalia
                , muacUnitTransIdForSite SiteUnknown
                )
                    |> Expect.equal
                        ( Translate.UnitCentimeter
                        , Translate.UnitCentimeter
                        , Translate.UnitCentimeter
                        )
        ]


all : Test
all =
    describe "Pages.Utils"
        [ percentageOfTotalTest
        , muacValueForSiteTest
        , muacUnitTransIdForSiteTest
        ]
