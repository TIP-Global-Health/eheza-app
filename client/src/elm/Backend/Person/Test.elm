module Backend.Person.Test exposing (all)

{-| What the person form offers and saves: the birth-date range offered when
registering a child, the age rule the save applies to the date picked, and
the national ID saved for the text typed, and the address and health centre
saved when a person is edited or a family member is added.

A child-only birth-date picker offers `earliestChildBirthDate currentDate` up
to `currentDate`, both ends selectable. An adult-only picker offers dates up
to exactly 13 years before `currentDate` (`Date.add Years -13`), inclusive.
On save, `isAdult` decides whether the birth date belongs to an adult, in
which case education level and marital status are required.

-}

import AssocList as Dict
import Backend.Entities exposing (HealthCenterId)
import Backend.Person.Form
    exposing
        ( applyDefaultValuesForPerson
        , birthDate
        , cell
        , district
        , educationLevel
        , emptyCreateForm
        , emptyEditForm
        , firstName
        , gender
        , healthCenter
        , maritalStatus
        , modeOfDelivery
        , phoneNumber
        , province
        , secondName
        , sector
        , validatePerson
        , village
        )
import Backend.Person.Model exposing (EducationLevel(..), Initiator(..), MaritalStatus(..), ModeOfDelivery(..), ParticipantDirectoryOperation(..), Person, Ubudehe(..))
import Backend.Person.Utils exposing (earliestChildBirthDate, graduatingAgeInMonth, isAdult)
import Date exposing (Interval(..), Unit(..))
import Expect
import Form exposing (Form)
import Form.Field
import GeoLocation.Model exposing (GeoLocation, GeoLocationId, ReverseGeoInfo)
import GeoLocation.Utils exposing (getGeoInfo, getReverseGeoInfo)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isJust)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import TestFixtures exposing (testChild, testPerson)
import Time exposing (Month(..))


date : Int -> Month -> Int -> NominalDate
date =
    Date.fromCalendarDate


{-| Exactly 13 years before the given date, the latest birth date an
adult-only picker offers.
-}
thirteenYearsAgo : NominalDate -> NominalDate
thirteenYearsAgo currentDate =
    Date.add Years -13 currentDate


{-| Every day from 2024-01-01 to 2026-12-31: a leap year and two common
years, so every month end, the leap day itself, the day after a common
February, and two year ends all appear as "today".
-}
sweep : List NominalDate
sweep =
    Date.range Day 1 (date 2024 Jan 1) (date 2027 Jan 1)


{-| The "today"s where the calendar does something unusual 13 years back.
Each is listed with the day it is, and why it matters.
-}
edgeDays : List ( String, NominalDate )
edgeDays =
    [ ( "an ordinary day", date 2026 Nov 5 )
    , ( "a leap day, whose 13-years-ago year has no 29 February", date 2024 Feb 29 )
    , ( "28 February of a common year, 13 years after a leap year", date 2025 Feb 28 )
    , ( "1 March of a common year, 13 years after a leap year", date 2025 Mar 1 )
    , ( "the last day of a 31-day month", date 2026 Mar 31 )
    , ( "the last day of a 30-day month", date 2026 Apr 30 )
    , ( "the last day of the year", date 2026 Dec 31 )
    , ( "the first day of the year", date 2026 Jan 1 )
    , ( "a leap day 13 years after a common year", date 2028 Feb 29 )
    ]


{-| Passes when the check holds on every day of `sweep`; otherwise fails
naming the first day it does not hold on.
-}
expectOnEveryDay : String -> (NominalDate -> Bool) -> Expect.Expectation
expectOnEveryDay description check =
    case List.filter (check >> not) sweep of
        [] ->
            Expect.pass

        first :: rest ->
            Expect.fail <|
                description
                    ++ " does not hold on "
                    ++ Date.toIsoString first
                    ++ " (and "
                    ++ String.fromInt (List.length rest)
                    ++ " other days in the sweep)"


{-| The names of the form fields that carry a validation error.
-}
fieldsWithErrors : Form e o -> List String
fieldsWithErrors =
    Form.getErrors >> List.map Tuple.first


{-| The errors a registration form shows once a birth date is entered, and
nothing else. `maybeRelated` is the person the new one is registered for;
`Nothing` is registration without a relation, as in Well Child.
-}
formErrorsFor : Site -> Maybe Person -> NominalDate -> NominalDate -> List String
formErrorsFor site maybeRelated currentDate birth =
    let
        operation =
            case maybeRelated of
                Just _ ->
                    CreatePerson (Just (toEntityUuid "related-person"))

                Nothing ->
                    CreatePerson Nothing
    in
    Form.initial
        [ ( Backend.Person.Form.firstName, Form.Field.string "Test" )
        , ( Backend.Person.Form.secondName, Form.Field.string "Person" )
        , ( birthDate, Form.Field.string (Date.toIsoString birth) )
        ]
        (validatePerson site maybeRelated operation (Just currentDate))
        |> fieldsWithErrors


{-| An adult a child is registered for: born 1985-01-01.
-}
relatedAdult : Person
relatedAdult =
    testPerson


{-| A child a parent is registered for: three years old on every date used.
-}
relatedChild : Person
relatedChild =
    testChild (date 2022 Jun 15)


all : Test
all =
    describe "Person registration"
        [ describe "Child birth-date range"
            [ wellChildEarliestDate
            , sameRangeForEveryChildPicker
            , everyOfferedDateSavesAsChild
            , exactThirteenYearsSavesAsAdult
            , graduatingAgeUnchanged
            ]
        , nationalIdNumberSaved
        , addressAndHealthCentre
        ]


{-| What the form saves as the national ID for the text typed into the field:
`Err` with the refused fields when the form does not save. The form is filled for an adult; `person` is
the one it takes the address and health centre from.
-}
savedNationalIdOn : ParticipantDirectoryOperation -> Person -> Site -> String -> Result (List String) (Maybe String)
savedNationalIdOn operation person site typed =
    let
        related =
            { person
                | province = Just "Province"
                , district = Just "District"
                , sector = Just "Sector"
                , cell = Just "Cell"
                , village = Just "Village"
                , healthCenterId = Just (toEntityUuid "health-center")
            }
    in
    Form.initial
        [ ( Backend.Person.Form.firstName, Form.Field.string "Test" )
        , ( Backend.Person.Form.secondName, Form.Field.string "Person" )
        , ( birthDate, Form.Field.string "1990-06-15" )
        , ( Backend.Person.Form.gender, Form.Field.string "female" )
        , ( educationLevel, Form.Field.string "0" )
        , ( maritalStatus, Form.Field.string "single" )
        , ( Backend.Person.Form.nationalIdNumber, Form.Field.string typed )
        ]
        (validatePerson site (Just related) operation (Just (date 2026 Nov 5)))
        |> (\form ->
                Form.getOutput form
                    |> Maybe.map .nationalIdNumber
                    |> Result.fromMaybe (fieldsWithErrors form)
           )


{-| Registering an adult for a child.
-}
savedNationalId : Site -> String -> Result (List String) (Maybe String)
savedNationalId =
    savedNationalIdOn (CreatePerson (Just (toEntityUuid "related-person"))) relatedChild


{-| Editing an adult.
-}
savedNationalIdOnEdit : Site -> String -> Result (List String) (Maybe String)
savedNationalIdOnEdit =
    savedNationalIdOn (EditPerson (toEntityUuid "edited-person")) relatedAdult


{-| Burundi and Somalia take any text as the ID. The spaces around it are
not saved, because the ID search matches from the start of the saved value.
Rwanda requires exactly 16 digits.
-}
nationalIdNumberSaved : Test
nationalIdNumberSaved =
    let
        freeTextSites =
            [ ( "Burundi", SiteBurundi ), ( "Somalia", SiteSomalia ) ]

        onFreeTextSites description typed expected =
            List.map
                (\( name, site ) ->
                    test (name ++ ": " ++ description) <|
                        \_ ->
                            savedNationalId site typed
                                |> Expect.equal (Ok expected)
                )
                freeTextSites
    in
    describe "the national ID saved" <|
        List.concat
            [ onFreeTextSites "a leading space is not saved" " 12/345" (Just "12/345")
            , onFreeTextSites "a trailing space is not saved" "12/345 " (Just "12/345")
            , onFreeTextSites "spaces at both ends, tabs and line breaks are not saved" " \t12/345 \n" (Just "12/345")
            , onFreeTextSites "spaces inside the ID are kept" " 12 345/A " (Just "12 345/A")
            , onFreeTextSites "an ID without outer spaces is saved as typed" "12/345" (Just "12/345")
            , onFreeTextSites "a field holding only spaces saves no ID" "   " Nothing
            , onFreeTextSites "an empty field saves no ID" "" Nothing
            , List.map
                (\( name, site ) ->
                    test (name ++ ": editing a person saves the ID without outer spaces too") <|
                        \_ ->
                            savedNationalIdOnEdit site " 12/345 "
                                |> Expect.equal (Ok (Just "12/345"))
                )
                freeTextSites
            , [ test "Rwanda: 16 digits are saved" <|
                    \_ ->
                        savedNationalId SiteRwanda "1199080012345678"
                            |> Expect.equal (Ok (Just "1199080012345678"))
              , test "Rwanda: 16 digits with spaces around them are saved without the spaces" <|
                    \_ ->
                        savedNationalId SiteRwanda " 1199080012345678 "
                            |> Expect.equal (Ok (Just "1199080012345678"))
              , test "Rwanda: 15 digits are refused" <|
                    \_ ->
                        savedNationalId SiteRwanda "119908001234567"
                            |> Expect.equal (Err [ Backend.Person.Form.nationalIdNumber ])
              , test "Rwanda: free text is refused" <|
                    \_ ->
                        savedNationalId SiteRwanda "12/345"
                            |> Expect.equal (Err [ Backend.Person.Form.nationalIdNumber ])
              , test "Rwanda: a field holding only spaces saves no ID" <|
                    \_ ->
                        savedNationalId SiteRwanda "   "
                            |> Expect.equal (Ok Nothing)
              , test "Rwanda: editing a person saves 16 digits without the spaces around them" <|
                    \_ ->
                        savedNationalIdOnEdit SiteRwanda " 1199080012345678 "
                            |> Expect.equal (Ok (Just "1199080012345678"))
              , test "Rwanda: editing a person refuses 15 digits" <|
                    \_ ->
                        savedNationalIdOnEdit SiteRwanda "119908001234567"
                            |> Expect.equal (Err [ Backend.Person.Form.nationalIdNumber ])
              , test "Rwanda: an empty field saves no ID" <|
                    \_ ->
                        savedNationalId SiteRwanda ""
                            |> Expect.equal (Ok Nothing)
              ]
            ]


{-| Criterion: when registering a child for Well Child, the earliest birth
date offered is 13 years ago plus one day; exactly 13 years ago is not
offered.
-}
wellChildEarliestDate : Test
wellChildEarliestDate =
    describe "the earliest birth date offered for a child"
        [ test "is 13 years ago plus one day" <|
            -- On 2026-11-05, 13 years ago is 2013-11-05, so the picker starts
            -- on 2013-11-06.
            \_ ->
                earliestChildBirthDate (date 2026 Nov 5)
                    |> Date.toIsoString
                    |> Expect.equal "2013-11-06"
        , test "exactly 13 years ago is not offered" <|
            \_ ->
                expectOnEveryDay "earliest child birth date is after 13 years ago"
                    (\today -> Date.compare (earliestChildBirthDate today) (thirteenYearsAgo today) == GT)
        , describe "around month and year ends"
            [ test "on 31 December it is 1 January of the following year, 13 years back" <|
                \_ ->
                    earliestChildBirthDate (date 2026 Dec 31)
                        |> Date.toIsoString
                        |> Expect.equal "2014-01-01"
            , test "on 31 March it is 1 April, 13 years back" <|
                \_ ->
                    earliestChildBirthDate (date 2026 Mar 31)
                        |> Date.toIsoString
                        |> Expect.equal "2013-04-01"
            , test "on 30 April it is 1 May, 13 years back" <|
                \_ ->
                    earliestChildBirthDate (date 2026 Apr 30)
                        |> Date.toIsoString
                        |> Expect.equal "2013-05-01"
            , test "on 1 January it is 2 January, 13 years back" <|
                \_ ->
                    earliestChildBirthDate (date 2026 Jan 1)
                        |> Date.toIsoString
                        |> Expect.equal "2013-01-02"
            ]
        , describe "around leap days"
            [ test "on 28 February 2025 it is the leap day 29 February 2012" <|
                -- 13 years before 2025-02-28 is 2012-02-28; the day after is
                -- 2012-02-29, which exists.
                \_ ->
                    earliestChildBirthDate (date 2025 Feb 28)
                        |> Date.toIsoString
                        |> Expect.equal "2012-02-29"
            , test "on 1 March 2025 it is 2 March 2012" <|
                \_ ->
                    earliestChildBirthDate (date 2025 Mar 1)
                        |> Date.toIsoString
                        |> Expect.equal "2012-03-02"
            , test "on the leap day 29 February 2024 it is 1 March 2011" <|
                -- 2011 has no 29 February, so 13 years before 2024-02-29 is
                -- 2011-02-28 (what the adult picker offers last), and the
                -- child picker starts the day after.
                \_ ->
                    earliestChildBirthDate (date 2024 Feb 29)
                        |> Date.toIsoString
                        |> Expect.equal "2011-03-01"
            ]
        ]


{-| Criterion: the same applies everywhere a child is registered with a
child-only date range: Nutrition, Family encounter, a child added to a related
adult, and the Sorwathe/ACHI group default.

Nutrition and Family encounter already start their range on "13 years ago,
plus one day". Every child-only range must start on that same date, every
day of the calendar, so that the child range begins the day after the adult
range ends: no birth date offered by both, none offered by neither.

-}
sameRangeForEveryChildPicker : Test
sameRangeForEveryChildPicker =
    describe "every child-only picker"
        [ test "starts on the same date as Nutrition and Family encounter: 13 years ago plus one day" <|
            \_ ->
                expectOnEveryDay "earliest child birth date is the day after 13 years ago"
                    (\today -> earliestChildBirthDate today == Date.add Days 1 (thirteenYearsAgo today))
        , test "starts the day after the last date an adult-only picker offers, on every edge day" <|
            \_ ->
                edgeDays
                    |> List.filter
                        (\( _, today ) ->
                            Date.diff Days (thirteenYearsAgo today) (earliestChildBirthDate today) /= 1
                        )
                    |> List.map Tuple.first
                    |> Expect.equalLists []
        ]


{-| Criterion: every birth date a child picker offers saves as a child,
without asking for education level or marital status.
-}
everyOfferedDateSavesAsChild : Test
everyOfferedDateSavesAsChild =
    describe "every birth date a child picker offers"
        [ test "saves as a child: the earliest date offered is not an adult's on any day" <|
            \_ ->
                expectOnEveryDay "the earliest child birth date counts as a child"
                    (\today -> isAdult today (Just (earliestChildBirthDate today)) == Just False)
        , test "saves as a child: every date offered on an edge day is a child's" <|
            -- The whole range, from the earliest date to today, on each of the
            -- calendar edge days.
            \_ ->
                edgeDays
                    |> List.concatMap
                        (\( name, today ) ->
                            Date.range Day 1 (earliestChildBirthDate today) (Date.add Days 1 today)
                                |> List.filter (\birth -> isAdult today (Just birth) /= Just False)
                                |> List.map (\birth -> name ++ ": " ++ Date.toIsoString birth)
                        )
                    |> Expect.equalLists []
        , test "saves without asking for education level or marital status (Well Child, no relation)" <|
            -- Well Child registers the child with no relation, so the save
            -- decides child or adult from the birth date alone. The earliest
            -- date offered must not make education level and marital status
            -- required.
            \_ ->
                edgeDays
                    |> List.filter
                        (\( _, today ) ->
                            formErrorsFor SiteRwanda Nothing today (earliestChildBirthDate today)
                                |> List.any (\field -> field == educationLevel || field == maritalStatus)
                        )
                    |> List.map Tuple.first
                    |> Expect.equalLists []
        , test "saves without asking for education level or marital status on a site that is not Rwanda" <|
            \_ ->
                formErrorsFor SiteBurundi Nothing (date 2026 Nov 5) (earliestChildBirthDate (date 2026 Nov 5))
                    |> List.filter (\field -> field == educationLevel || field == maritalStatus)
                    |> Expect.equalLists []
        , test "saves as a child of a related adult, without an error on the birth date" <|
            -- A child added to a related adult is validated as a child; the
            -- earliest date offered must not be refused as too old.
            \_ ->
                edgeDays
                    |> List.filter
                        (\( _, today ) ->
                            formErrorsFor SiteRwanda (Just relatedAdult) today (earliestChildBirthDate today)
                                |> List.member birthDate
                        )
                    |> List.map Tuple.first
                    |> Expect.equalLists []
        , test "today, the latest date offered, saves as a child" <|
            \_ ->
                isAdult (date 2026 Nov 5) (Just (date 2026 Nov 5))
                    |> Expect.equal (Just False)
        ]


{-| Criterion: a birth date of exactly 13 years ago stays selectable where an
adult is registered, and saves as an adult.

The adult picker's bound is built in the view, not in the seam, so these
tests pin the half that is pure: that date is an adult's on save.

-}
exactThirteenYearsSavesAsAdult : Test
exactThirteenYearsSavesAsAdult =
    describe "a birth date of exactly 13 years ago"
        [ test "saves as an adult, on every day" <|
            \_ ->
                expectOnEveryDay "exactly 13 years ago counts as an adult"
                    (\today -> isAdult today (Just (thirteenYearsAgo today)) == Just True)
        , test "the day after it saves as a child, on every day" <|
            \_ ->
                expectOnEveryDay "the day after 13 years ago counts as a child"
                    (\today -> isAdult today (Just (Date.add Days 1 (thirteenYearsAgo today))) == Just False)
        , test "saves as an adult with no relation: education level and marital status are required" <|
            \_ ->
                formErrorsFor SiteRwanda Nothing (date 2026 Nov 5) (thirteenYearsAgo (date 2026 Nov 5))
                    |> List.filter (\field -> field == educationLevel || field == maritalStatus)
                    |> List.sort
                    |> Expect.equalLists (List.sort [ educationLevel, maritalStatus ])
        , test "saves as a parent of a related child, without an error on the birth date" <|
            \_ ->
                edgeDays
                    |> List.filter
                        (\( _, today ) ->
                            formErrorsFor SiteRwanda (Just relatedChild) today (thirteenYearsAgo today)
                                |> List.member birthDate
                        )
                    |> List.map Tuple.first
                    |> Expect.equalLists []
        , test "with no birth date, neither adult nor child is decided" <|
            \_ ->
                isAdult (date 2026 Nov 5) Nothing
                    |> Expect.equal Nothing
        ]


{-| Criterion: the graduating-age limit for other group types is unchanged.
The picker for a child added in such a group starts this many months back.
-}
graduatingAgeUnchanged : Test
graduatingAgeUnchanged =
    test "the graduating-age limit for other group types is unchanged: 26 months" <|
        \_ ->
            graduatingAgeInMonth
                |> Expect.equal 26



-- ADDRESS AND HEALTH CENTRE ON SAVE


{-| The day every address scenario is saved on.
-}
saveDate : NominalDate
saveDate =
    date 2026 Nov 18


{-| One place on the device's address list: its ID, which is what the form's
select holds, and its name and parent.
-}
type alias Place =
    ( GeoLocationId, GeoLocation )


{-| A full address on the device's address list: one place per level, each
inside the one before it.
-}
type alias Address =
    { province : Place
    , district : Place
    , sector : Place
    , cell : Place
    , village : Place
    }


{-| The places the address scenarios need on one site.

`home` is where the person lives. `otherCell` is an address in the same
sector as `home` but in another cell; `otherSector` is in the same district
but another sector. Somalia has a single cell per sector, so its `otherCell`
is `Nothing`.

-}
type alias Places =
    { home : Address
    , otherCell : Maybe Address
    , otherSector : Maybe Address
    }


{-| Every full address on the site's address list.
-}
addressesOn : Site -> List Address
addressesOn site =
    let
        geoInfo =
            getGeoInfo site

        childrenOf level =
            Dict.foldr
                (\id place ->
                    Dict.update place.parent
                        (Maybe.withDefault [] >> (::) ( id, place ) >> Just)
                )
                Dict.empty
                level

        districts =
            childrenOf geoInfo.districts

        sectors =
            childrenOf geoInfo.sectors

        cells =
            childrenOf geoInfo.cells

        villages =
            childrenOf geoInfo.villages

        under children ( parentId, _ ) =
            Dict.get (Just parentId) children
                |> Maybe.withDefault []
    in
    Dict.toList geoInfo.provinces
        |> List.concatMap
            (\province_ ->
                under districts province_
                    |> List.concatMap
                        (\district_ ->
                            under sectors district_
                                |> List.concatMap
                                    (\sector_ ->
                                        under cells sector_
                                            |> List.concatMap
                                                (\cell_ ->
                                                    under villages cell_
                                                        |> List.map (Address province_ district_ sector_ cell_)
                                                )
                                    )
                        )
            )


placesOn : Site -> Maybe Places
placesOn site =
    let
        addresses =
            addressesOn site

        samePlace a b =
            Tuple.first a == Tuple.first b

        otherCellFor home =
            List.Extra.find
                (\address -> samePlace address.sector home.sector && not (samePlace address.cell home.cell))
                addresses

        otherSectorFor home =
            List.Extra.find
                (\address -> samePlace address.district home.district && not (samePlace address.sector home.sector))
                addresses

        withBoth home =
            Maybe.map2 (\otherCell otherSector -> Places home (Just otherCell) (Just otherSector))
                (otherCellFor home)
                (otherSectorFor home)

        withOtherSector home =
            Maybe.map (\otherSector -> Places home Nothing (Just otherSector))
                (otherSectorFor home)
    in
    case List.Extra.findMap withBoth addresses of
        Just places ->
            Just places

        Nothing ->
            List.Extra.findMap withOtherSector addresses


rwandaPlaces : Maybe Places
rwandaPlaces =
    placesOn SiteRwanda


burundiPlaces : Maybe Places
burundiPlaces =
    placesOn SiteBurundi


somaliaPlaces : Maybe Places
somaliaPlaces =
    placesOn SiteSomalia


rwandaReverseGeoInfo : ReverseGeoInfo
rwandaReverseGeoInfo =
    getReverseGeoInfo (getGeoInfo SiteRwanda)


burundiReverseGeoInfo : ReverseGeoInfo
burundiReverseGeoInfo =
    getReverseGeoInfo (getGeoInfo SiteBurundi)


somaliaReverseGeoInfo : ReverseGeoInfo
somaliaReverseGeoInfo =
    getReverseGeoInfo (getGeoInfo SiteSomalia)


reverseGeoInfoOn : Site -> ReverseGeoInfo
reverseGeoInfoOn site =
    case site of
        SiteBurundi ->
            burundiReverseGeoInfo

        SiteRwanda ->
            rwandaReverseGeoInfo

        SiteSomalia ->
            somaliaReverseGeoInfo

        _ ->
            getReverseGeoInfo (getGeoInfo site)


{-| Runs the check with the site's places, or fails when the site's address
list has none to offer.
-}
withPlaces : Site -> (Places -> Expect.Expectation) -> Expect.Expectation
withPlaces site check =
    let
        places =
            case site of
                SiteBurundi ->
                    burundiPlaces

                SiteRwanda ->
                    rwandaPlaces

                SiteSomalia ->
                    somaliaPlaces

                _ ->
                    placesOn site
    in
    case places of
        Just found ->
            check found

        Nothing ->
            Expect.fail "the site's address list has no address these scenarios can use"


{-| The sites every address scenario runs on.
-}
sites : List ( String, Site )
sites =
    [ ( "Rwanda", SiteRwanda ), ( "Burundi", SiteBurundi ), ( "Somalia", SiteSomalia ) ]


{-| One test per site, named after it.
-}
onEverySite : String -> (Site -> Expect.Expectation) -> Test
onEverySite description check =
    describe description <|
        List.map (\( name, site ) -> test name <| \_ -> check site) sites


placeName : Place -> String
placeName ( _, place ) =
    place.name


{-| What the form's select holds when the place is picked.
-}
placeId : Place -> String
placeId ( id, _ ) =
    String.fromInt (fromEntityId id)


homeHealthCentre : HealthCenterId
homeHealthCentre =
    toEntityUuid "home-health-centre"


{-| An adult who lives at the address and is registered at
`homeHealthCentre`. Education level and marital status are set, since an
adult's save requires them.
-}
personAt : Address -> Person
personAt address =
    { testPerson
        | province = Just (placeName address.province)
        , district = Just (placeName address.district)
        , sector = Just (placeName address.sector)
        , cell = Just (placeName address.cell)
        , village = Just (placeName address.village)
        , healthCenterId = Just homeHealthCentre
        , educationLevel = Just PrimarySchool
        , maritalStatus = Just Married
        , ubudehe = Just Ubudehe2
        , telephoneNumber = Just "0788000000"
    }


{-| The part of a person the address scenarios look at.
-}
type alias SavedAddress =
    { province : Maybe String
    , district : Maybe String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
    , healthCenterId : Maybe HealthCenterId
    }


{-| The saved address, written out for a failure message.
-}
describeSaved : SavedAddress -> String
describeSaved saved =
    [ saved.province, saved.district, saved.sector, saved.cell, saved.village, Maybe.map fromEntityUuid saved.healthCenterId ]
        |> List.map (Maybe.withDefault "(none)")
        |> String.join " / "


savedAddress : Person -> SavedAddress
savedAddress person =
    { province = person.province
    , district = person.district
    , sector = person.sector
    , cell = person.cell
    , village = person.village
    , healthCenterId = person.healthCenterId
    }


pick : String -> String -> Form.Msg
pick name value =
    Form.Input name Form.Select (Form.Field.String value)


{-| The nurse sets a select back to its empty option.
-}
clear : String -> Form.Msg
clear name =
    pick name ""


typeText : String -> String -> Form.Msg
typeText name value =
    Form.Input name Form.Text (Form.Field.String value)


{-| The nurse opens the form, makes the given changes, and presses Save. This
follows the person page: the form keeps only what the nurse changed, and the
related person's values are filled into the fields still empty, both on
screen and on Save. `Ok` is the person saved; `Err` lists the fields shown in
error, sorted, and nothing is saved.
-}
pressSave : Site -> ParticipantDirectoryOperation -> Person -> List Form.Msg -> Result (List String) Person
pressSave site operation related changes =
    let
        validation =
            validatePerson site (Just related) operation (Just saveDate)

        opened =
            case operation of
                CreatePerson _ ->
                    emptyCreateForm site

                EditPerson _ ->
                    emptyEditForm site

        changed =
            List.foldl (Form.update validation) opened changes

        withDefaults =
            applyDefaultValuesForPerson saveDate
                site
                (reverseGeoInfoOn site)
                Nothing
                False
                (Just related)
                operation
                ParticipantDirectoryOrigin
    in
    case Form.getOutput (withDefaults changed) of
        Just person ->
            Ok person

        Nothing ->
            let
                shown =
                    withDefaults (Form.update validation Form.Submit changed)
            in
            Form.getErrors shown
                |> List.map Tuple.first
                |> List.filter (\name -> isJust (Form.getFieldAsString name shown).liveError)
                |> List.sort
                |> Err


editPerson : ParticipantDirectoryOperation
editPerson =
    EditPerson (toEntityUuid "edited-person")


{-| The nurse edits the person, makes the changes and presses Save.
-}
saveEdit : Site -> Person -> List Form.Msg -> Result (List String) SavedAddress
saveEdit site person changes =
    pressSave site editPerson person changes
        |> Result.map savedAddress


{-| The fields shown as required when the save is refused, or a failure
naming the address that was saved instead.
-}
expectRequired : List String -> Result (List String) SavedAddress -> Expect.Expectation
expectRequired fields result =
    case result of
        Err shown ->
            Expect.equalLists (List.sort fields) shown

        Ok saved ->
            Expect.fail <|
                "nothing should be saved until "
                    ++ String.join ", " fields
                    ++ " is picked, but the form saved "
                    ++ describeSaved saved


{-| The cleared field is among those shown as required, and nothing is
saved.
-}
expectAmongRequired : String -> Result (List String) SavedAddress -> Expect.Expectation
expectAmongRequired field result =
    case result of
        Err shown ->
            if List.member field shown then
                Expect.pass

            else
                Expect.fail <| field ++ " is not shown as required; the fields in error are " ++ String.join ", " shown

        Ok saved ->
            Expect.fail <|
                "nothing should be saved until "
                    ++ field
                    ++ " is picked, but the form saved "
                    ++ describeSaved saved


addressLevels : List String
addressLevels =
    [ province, district, sector, cell, village ]


{-| The same address and health centre as the person's, as saved.
-}
unchanged : Person -> Result (List String) SavedAddress
unchanged person =
    Ok (savedAddress person)


addressAndHealthCentre : Test
addressAndHealthCentre =
    describe "the address and health centre saved"
        [ clearedVillageIsRequired
        , everyClearedLevelIsRequired
        , changedCellNeedsAVillage
        , untouchedAddressSavesUnchanged
        , unlistedAddressSavesWhenUntouched
        , familyMemberAddress
        ]


{-| Criterion: editing a person, clearing the village and saving shows the
village as required. Nothing is saved until a village is picked.
-}
clearedVillageIsRequired : Test
clearedVillageIsRequired =
    describe "Editing a person, clearing the village and saving"
        [ onEverySite "shows the village as required, and nothing is saved until a village is picked" <|
            \site ->
                withPlaces site <|
                    \places ->
                        saveEdit site (personAt places.home) [ clear village ]
                            |> expectRequired [ village ]
        , onEverySite "then picking the same village again saves the address unchanged" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            person =
                                personAt places.home
                        in
                        saveEdit site person [ clear village, pick village (placeId places.home.village) ]
                            |> Expect.equal (unchanged person)
        , test "Rwanda: also editing the phone number does not let the cleared village through" <|
            \_ ->
                withPlaces SiteRwanda <|
                    \places ->
                        saveEdit SiteRwanda (personAt places.home) [ typeText phoneNumber "0788111111", clear village ]
                            |> expectRequired [ village ]
        , test "Rwanda: clearing the village of a child shows it as required too" <|
            -- The village select is locked for a child being edited, but the
            -- save must not depend on that lock.
            \_ ->
                withPlaces SiteRwanda <|
                    \places ->
                        let
                            child =
                                personAt places.home
                                    |> (\person -> { person | birthDate = Just (date 2023 Mar 10), educationLevel = Nothing, maritalStatus = Nothing, modeOfDelivery = Just CesareanDelivery })
                        in
                        saveEdit SiteRwanda child [ clear village ]
                            |> expectRequired [ village ]
        ]


{-| Criterion: the same holds for every address level, and for the health
centre.

The address selects lock a level until the levels below it are cleared, so on
screen clearing the province means clearing all five. The save must not rely
on that lock: each level cleared alone is shown as required too.

-}
everyClearedLevelIsRequired : Test
everyClearedLevelIsRequired =
    describe "The same holds for every address level, and for the health centre"
        [ describe "a level cleared alone is shown as required, and nothing is saved" <|
            List.map
                (\level ->
                    onEverySite level <|
                        \site ->
                            withPlaces site <|
                                \places ->
                                    saveEdit site (personAt places.home) [ clear level ]
                                        |> expectAmongRequired level
                )
                [ province, district, sector, cell ]
        , onEverySite "clearing the levels from the village up to the province shows all five as required" <|
            \site ->
                withPlaces site <|
                    \places ->
                        saveEdit site (personAt places.home) (List.map clear (List.reverse addressLevels))
                            |> expectRequired addressLevels
        , onEverySite "clearing the village and the cell shows both as required" <|
            \site ->
                withPlaces site <|
                    \places ->
                        saveEdit site (personAt places.home) [ clear village, clear cell ]
                            |> expectRequired [ cell, village ]
        , onEverySite "clearing the health centre shows it as required, and nothing is saved" <|
            \site ->
                withPlaces site <|
                    \places ->
                        saveEdit site (personAt places.home) [ clear healthCenter ]
                            |> expectRequired [ healthCenter ]
        , onEverySite "picking another health centre saves it" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            person =
                                personAt places.home

                            otherHealthCentre =
                                toEntityUuid "other-health-centre"
                        in
                        saveEdit site person [ pick healthCenter (fromEntityUuid otherHealthCentre) ]
                            |> Expect.equal (Ok (savedAddress { person | healthCenterId = Just otherHealthCentre }))
        ]


{-| Criterion: changing the cell and saving without picking a village shows
the village as required, so a new cell can never be saved with the old
village.

On screen the cell is locked while a village is set, so the nurse clears the
village, then the cell, then picks the new cell.

-}
changedCellNeedsAVillage : Test
changedCellNeedsAVillage =
    let
        changeCell otherCell =
            [ clear village, clear cell, pick cell (placeId otherCell.cell) ]

        changeSector otherSector =
            [ clear village, clear cell, clear sector, pick sector (placeId otherSector.sector) ]

        onSitesWithAnotherCell description check =
            describe description <|
                List.map
                    (\( name, site ) ->
                        test name <|
                            \_ ->
                                withPlaces site <|
                                    \places ->
                                        case places.otherCell of
                                            Just otherCell ->
                                                check site places otherCell

                                            Nothing ->
                                                Expect.fail "no sector on this site has two cells"
                    )
                    [ ( "Rwanda", SiteRwanda ), ( "Burundi", SiteBurundi ) ]

        onEverySiteWithAnotherSector description check =
            onEverySite description <|
                \site ->
                    withPlaces site <|
                        \places ->
                            case places.otherSector of
                                Just otherSector ->
                                    check site places otherSector

                                Nothing ->
                                    Expect.fail "no district on this site has two sectors"
    in
    describe "Changing the cell and saving without picking a village"
        [ onSitesWithAnotherCell "shows the village as required, so a new cell can never be saved with the old village" <|
            \site places otherCell ->
                saveEdit site (personAt places.home) (changeCell otherCell)
                    |> expectRequired [ village ]
        , onSitesWithAnotherCell "picking the same cell again without a village shows the village as required" <|
            \site places _ ->
                saveEdit site (personAt places.home) [ clear village, clear cell, pick cell (placeId places.home.cell) ]
                    |> expectRequired [ village ]
        , onSitesWithAnotherCell "picking a village of the new cell saves the new cell with that village" <|
            \site places otherCell ->
                let
                    person =
                        personAt places.home
                in
                saveEdit site person (changeCell otherCell ++ [ pick village (placeId otherCell.village) ])
                    |> Expect.equal
                        (Ok
                            (savedAddress
                                { person
                                    | cell = Just (placeName otherCell.cell)
                                    , village = Just (placeName otherCell.village)
                                }
                            )
                        )
        , onEverySiteWithAnotherSector "changing the sector and picking a cell without a village shows the village as required" <|
            \site places otherSector ->
                saveEdit site (personAt places.home) (changeSector otherSector ++ [ pick cell (placeId otherSector.cell) ])
                    |> expectRequired [ village ]
        , onEverySiteWithAnotherSector "changing the sector without picking a cell shows the cell and the village as required" <|
            \site places otherSector ->
                saveEdit site (personAt places.home) (changeSector otherSector)
                    |> expectRequired [ cell, village ]
        , onSitesWithAnotherCell "a person whose stored village is missing from the address list: changing the cell still needs a village" <|
            -- The stored village cannot be shown in the select, so the
            -- village field starts empty. Once the cell changes, that stored
            -- village must not be saved with the new cell either.
            \site places otherCell ->
                let
                    person =
                        personAt places.home
                            |> (\atHome -> { atHome | village = Just "Village missing from the list" })
                in
                saveEdit site person [ clear cell, pick cell (placeId otherCell.cell) ]
                    |> expectRequired [ village ]
        , onSitesWithAnotherCell "a person whose stored cell is missing from the address list: picking a cell still needs a village" <|
            -- Neither the stored cell nor its village can be shown, so both
            -- fields start empty. The nurse picks a cell and presses Save.
            \site places otherCell ->
                let
                    person =
                        personAt places.home
                            |> (\atHome -> { atHome | cell = Just "Cell missing from the list" })
                in
                saveEdit site person [ pick cell (placeId otherCell.cell) ]
                    |> expectRequired [ village ]
        ]


{-| Criterion: editing any other field of a person, without touching the
address, saves the address unchanged.
-}
untouchedAddressSavesUnchanged : Test
untouchedAddressSavesUnchanged =
    describe "Editing any other field of a person, without touching the address"
        [ onEverySite "saves the address unchanged when the phone number is edited" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            person =
                                personAt places.home
                        in
                        saveEdit site person [ typeText phoneNumber "0788111111" ]
                            |> Expect.equal (unchanged person)
        , onEverySite "saves the address unchanged when the first name is edited" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            person =
                                personAt places.home
                        in
                        saveEdit site person [ typeText firstName "Renamed" ]
                            |> Expect.equal (unchanged person)
        , onEverySite "saves the address unchanged when nothing is edited" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            person =
                                personAt places.home
                        in
                        saveEdit site person []
                            |> Expect.equal (unchanged person)
        , test "Rwanda: saves the phone number that was edited" <|
            \_ ->
                withPlaces SiteRwanda <|
                    \places ->
                        pressSave SiteRwanda editPerson (personAt places.home) [ typeText phoneNumber "0788111111" ]
                            |> Result.map .telephoneNumber
                            |> Expect.equal (Ok (Just "0788111111"))
        ]


{-| Criterion: a person whose stored address names a place missing from the
device's address list still saves when the address is not touched, and keeps
that address.

A missing place cannot be shown in its select, and neither can any level
below it, since each select offers only the places inside the one above.

-}
unlistedAddressSavesWhenUntouched : Test
unlistedAddressSavesWhenUntouched =
    let
        keepsAddress description withUnlisted =
            onEverySite description <|
                \site ->
                    withPlaces site <|
                        \places ->
                            let
                                person =
                                    withUnlisted (personAt places.home)
                            in
                            saveEdit site person [ typeText phoneNumber "0788111111" ]
                                |> Expect.equal (unchanged person)
    in
    describe "A person whose stored address names a place missing from the device's address list"
        [ keepsAddress "still saves when the address is not touched, and keeps a village missing from the list" <|
            \person -> { person | village = Just "Village missing from the list" }
        , keepsAddress "keeps a cell missing from the list, and the village stored under it" <|
            \person -> { person | cell = Just "Cell missing from the list" }
        , keepsAddress "keeps a sector missing from the list, and the levels stored under it" <|
            \person -> { person | sector = Just "Sector missing from the list" }
        , keepsAddress "keeps an address where no level is on the list" <|
            \person ->
                { person
                    | province = Just "Province missing from the list"
                    , district = Just "District missing from the list"
                    , sector = Just "Sector missing from the list"
                    , cell = Just "Cell missing from the list"
                    , village = Just "Village missing from the list"
                }
        , onEverySite "still saves, and keeps the address, when nothing at all is edited" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            person =
                                personAt places.home
                                    |> (\atHome -> { atHome | village = Just "Village missing from the list" })
                        in
                        saveEdit site person []
                            |> Expect.equal (unchanged person)
        ]


{-| Criterion: adding a family member, the address and health centre are
filled in from the related person, and saving them unchanged works. Clearing
a level shows it as required.

The family member added is a child of the related adult, so the save asks
for the mode of delivery and not for education level or marital status.

-}
familyMemberAddress : Test
familyMemberAddress =
    let
        addFamilyMember =
            CreatePerson (Just (toEntityUuid "related-person"))

        newChild =
            [ typeText firstName "New"
            , typeText secondName "Child"
            , typeText birthDate "2024-03-10"
            , pick gender "male"
            , pick modeOfDelivery "cesarean-delivery"
            ]

        saveFamilyMember site related changes =
            pressSave site addFamilyMember related (newChild ++ changes)
                |> Result.map savedAddress

        filledIn site related =
            applyDefaultValuesForPerson saveDate
                site
                (reverseGeoInfoOn site)
                Nothing
                False
                (Just related)
                addFamilyMember
                ParticipantDirectoryOrigin
                (emptyCreateForm site)
                |> (\form ->
                        List.map (\name -> ( name, (Form.getFieldAsString name form).value ))
                            (addressLevels ++ [ healthCenter ])
                   )
    in
    describe "Adding a family member"
        [ onEverySite "the address and health centre are filled in from the related person" <|
            \site ->
                withPlaces site <|
                    \places ->
                        filledIn site (personAt places.home)
                            |> Expect.equalLists
                                [ ( province, Just (placeId places.home.province) )
                                , ( district, Just (placeId places.home.district) )
                                , ( sector, Just (placeId places.home.sector) )
                                , ( cell, Just (placeId places.home.cell) )
                                , ( village, Just (placeId places.home.village) )
                                , ( healthCenter, Just (fromEntityUuid homeHealthCentre) )
                                ]
        , onEverySite "saving the address and health centre unchanged works" <|
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            related =
                                personAt places.home
                        in
                        saveFamilyMember site related []
                            |> Expect.equal (unchanged related)
        , describe "clearing a level shows it as required, and nothing is saved" <|
            List.map
                (\level ->
                    onEverySite level <|
                        \site ->
                            withPlaces site <|
                                \places ->
                                    saveFamilyMember site (personAt places.home) [ clear level ]
                                        |> expectAmongRequired level
                )
                addressLevels
        , onEverySite "clearing the village shows only the village as required" <|
            \site ->
                withPlaces site <|
                    \places ->
                        saveFamilyMember site (personAt places.home) [ clear village ]
                            |> expectRequired [ village ]
        , onEverySite "clearing the health centre shows it as required, and nothing is saved" <|
            \site ->
                withPlaces site <|
                    \places ->
                        saveFamilyMember site (personAt places.home) [ clear healthCenter ]
                            |> expectRequired [ healthCenter ]
        , onEverySite "the related person's stored village missing from the list: saving the address unchanged keeps it" <|
            -- The village cannot be filled in, as it is not in the select.
            -- The nurse leaves the address as it is and presses Save.
            \site ->
                withPlaces site <|
                    \places ->
                        let
                            related =
                                personAt places.home
                                    |> (\atHome -> { atHome | village = Just "Village missing from the list" })
                        in
                        saveFamilyMember site related []
                            |> Expect.equal (unchanged related)
        ]
