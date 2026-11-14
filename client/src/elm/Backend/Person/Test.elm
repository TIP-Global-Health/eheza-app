module Backend.Person.Test exposing (all)

{-| What the person form offers and saves: the birth-date range offered when
registering a child, the age rule the save applies to the date picked, and
the national ID saved for the text typed.

A child-only birth-date picker offers `earliestChildBirthDate currentDate` up
to `currentDate`, both ends selectable. An adult-only picker offers dates up
to exactly 13 years before `currentDate` (`Date.add Years -13`), inclusive.
On save, `isAdult` decides whether the birth date belongs to an adult, in
which case education level and marital status are required.

-}

import Backend.Person.Form
    exposing
        ( birthDate
        , educationLevel
        , maritalStatus
        , validatePerson
        )
import Backend.Person.Model exposing (ParticipantDirectoryOperation(..), Person)
import Backend.Person.Utils exposing (earliestChildBirthDate, graduatingAgeInMonth, isAdult)
import Date exposing (Interval(..), Unit(..))
import Expect
import Form exposing (Form)
import Form.Field
import Gizra.NominalDate exposing (NominalDate)
import Restful.Endpoint exposing (toEntityUuid)
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
