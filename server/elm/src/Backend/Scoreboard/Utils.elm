module Backend.Scoreboard.Utils exposing (generateECDMilestonesStatusByMonth, vaccineDoseFromOrder, vaccineDoseToComparable)

import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (..)
import Date exposing (Unit(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import List.Extra


vaccineDoseToComparable : VaccineDose -> Int
vaccineDoseToComparable dose =
    case dose of
        VaccineDoseFirst ->
            1

        VaccineDoseSecond ->
            2

        VaccineDoseThird ->
            3

        VaccineDoseFourth ->
            4

        VaccineDoseFifth ->
            5


vaccineDoseFromOrder : Int -> Maybe VaccineDose
vaccineDoseFromOrder order =
    case order of
        0 ->
            Just VaccineDoseFirst

        1 ->
            Just VaccineDoseSecond

        2 ->
            Just VaccineDoseThird

        3 ->
            Just VaccineDoseFourth

        4 ->
            Just VaccineDoseFifth

        _ ->
            Nothing


{-| Structure that holds monthly status of ECD milestones,
when child is between 0 and 24 months old.
-}
generateECDMilestonesStatusByMonth :
    NominalDate
    -> NominalDate
    -> List ECDEncounterData
    -> List ECDStatus
generateECDMilestonesStatusByMonth currentDate birthDate encountersData =
    let
        milestonesToCurrentDateWithStatus =
            generateECDMilestonesWithStatus currentDate
                birthDate
                encountersData
                |> Dict.fromList

        statusForMilestonePeriod ( milestone, status ) =
            case milestone of
                -- Covers age of 2 and 3 months.
                Milestone6Weeks ->
                    List.repeat 2 status

                -- Covers age of 4 and 5 months.
                Milestone14Weeks ->
                    List.repeat 2 status

                -- Covers age of 6, 7 and 8 months.
                Milestone6Months ->
                    List.repeat 3 status

                -- Covers age of 9, 10 and 11 months.
                Milestone9Months ->
                    List.repeat 3 status

                -- Covers age of 12, 13 and 14 months.
                Milestone12Months ->
                    List.repeat 3 status

                --    Covers age of 15, 16 and 17 months.
                Milestone15Months ->
                    List.repeat 3 status

                --    Covers age of 18 to 23 months.
                Milestone18Months ->
                    List.repeat 6 status

                --    Covers age of 24 and 25 months.
                Milestone2Years ->
                    List.repeat 2 status

                -- Not in range.
                Milestone3Years ->
                    []

                -- Not in range.
                Milestone4Years ->
                    []

        allMilestones =
            [ Milestone6Weeks
            , Milestone14Weeks
            , Milestone6Months
            , Milestone9Months
            , Milestone12Months
            , Milestone15Months
            , Milestone18Months
            , Milestone2Years
            ]
    in
    -- For first month, there's no ECD milestone.
    NoECDStatus
        :: (List.map
                (\milestone ->
                    ( milestone
                    , Dict.get milestone milestonesToCurrentDateWithStatus
                        |> Maybe.withDefault NoECDStatus
                    )
                )
                allMilestones
                |> List.map statusForMilestonePeriod
                |> List.concat
           )


generateECDMilestonesWithStatus :
    NominalDate
    -> NominalDate
    -> List ECDEncounterData
    -> List ( PediatricCareMilestone, ECDStatus )
generateECDMilestonesWithStatus currentDate birthDate encountersData =
    let
        milestoneForCurrentDateAsComparable =
            resolvePediatricCareMilestoneOnDate currentDate birthDate
                |> Maybe.map pediatricCareMilestoneToComparable

        milestonesToCurrentDate =
            Maybe.map
                (\currentMilestoneAsComparable ->
                    List.filter
                        (\milestone ->
                            pediatricCareMilestoneToComparable milestone <= currentMilestoneAsComparable
                        )
                        pediatricCareMilestones
                )
                milestoneForCurrentDateAsComparable
                |> Maybe.withDefault []

        performedMilestonesWithStatus =
            List.filterMap
                (\encounterData ->
                    let
                        milestoneStatus =
                            if encounterData.warning == WarningECDMilestoneReferToSpecialist then
                                StatusOffTrack

                            else if encounterData.warning == WarningECDMilestoneBehind then
                                StatusECDBehind

                            else
                                StatusOnTrack
                    in
                    resolvePediatricCareMilestoneOnDate encounterData.date birthDate
                        |> Maybe.map (\milestone -> ( milestone, milestoneStatus ))
                )
                encountersData
                |> Dict.fromList
    in
    List.map
        (\milestone ->
            let
                status =
                    Dict.get milestone performedMilestonesWithStatus
                        |> Maybe.withDefault (genrateDefaultECDStatus birthDate milestone encountersData)
            in
            ( milestone, status )
        )
        milestonesToCurrentDate


resolvePediatricCareMilestoneOnDate : NominalDate -> NominalDate -> Maybe PediatricCareMilestone
resolvePediatricCareMilestoneOnDate dueDate birthDate =
    let
        ageWeeks =
            Date.diff Weeks birthDate dueDate

        ageMonths =
            Date.diff Months birthDate dueDate
    in
    if ageWeeks < 6 then
        Nothing

    else if ageWeeks < 14 then
        Just Milestone6Weeks

    else if ageMonths < 6 then
        Just Milestone14Weeks

    else if ageMonths < 9 then
        Just Milestone6Months

    else if ageMonths < 12 then
        Just Milestone9Months

    else if ageMonths < 15 then
        Just Milestone12Months

    else if ageMonths < 18 then
        Just Milestone15Months

    else if ageMonths < 24 then
        Just Milestone18Months

    else if ageMonths < 36 then
        Just Milestone2Years

    else if ageMonths < 48 then
        Just Milestone3Years

    else
        Just Milestone4Years


genrateDefaultECDStatus :
    NominalDate
    -> PediatricCareMilestone
    -> List ECDEncounterData
    -> ECDStatus
genrateDefaultECDStatus birthDate milestone encountersData =
    let
        milestoneDate =
            resolveDateForPediatricCareMilestone birthDate milestone

        firstEncounterDateAfterMilestone =
            List.filterMap
                (\encounterData ->
                    if not <| Date.compare milestoneDate encounterData.date == LT then
                        Just encounterData.date

                    else
                        Nothing
                )
                encountersData
                |> List.sortWith Date.compare
                |> List.head

        -- Take all signs that were taken before the milestone,
        -- and, these of first encounter after milestone.
        completedSigns =
            Maybe.map
                (\firstEncounterAfterMilestoneDate ->
                    List.filterMap
                        (\encounterData ->
                            if Date.compare encounterData.date firstEncounterAfterMilestoneDate == GT then
                                Nothing

                            else
                                Just encounterData.signs
                        )
                        encountersData
                )
                firstEncounterDateAfterMilestone
                |> Maybe.withDefault
                    -- There were no encounters after milestone date, so we just
                    -- take all existing signs.
                    (List.map .signs encountersData)
                |> List.concat
                |> List.filter ((/=) NoECDSigns)
                |> EverySet.fromList

        expectedSigns =
            expectedECDSignsOnMilestone birthDate milestoneDate firstEncounterDateAfterMilestone
    in
    if List.all (\sign -> EverySet.member sign completedSigns) expectedSigns then
        StatusOnTrack

    else
        StatusOffTrack


resolveDateForPediatricCareMilestone : NominalDate -> PediatricCareMilestone -> NominalDate
resolveDateForPediatricCareMilestone birthDate milestone =
    case milestone of
        Milestone6Weeks ->
            Date.add Weeks 6 birthDate

        Milestone14Weeks ->
            Date.add Weeks 14 birthDate

        Milestone6Months ->
            Date.add Months 6 birthDate

        Milestone9Months ->
            Date.add Months 9 birthDate

        Milestone12Months ->
            Date.add Years 1 birthDate

        Milestone15Months ->
            Date.add Months 15 birthDate

        Milestone18Months ->
            Date.add Months 18 birthDate

        Milestone2Years ->
            Date.add Years 2 birthDate

        Milestone3Years ->
            Date.add Years 3 birthDate

        Milestone4Years ->
            Date.add Years 4 birthDate


expectedECDSignsOnMilestone : NominalDate -> NominalDate -> Maybe NominalDate -> List ECDSign
expectedECDSignsOnMilestone birthDate milestoneDate firstEncounterDateAfterMilestone =
    let
        ageWeeks =
            Date.diff Weeks birthDate milestoneDate

        ageMonths =
            Date.diff Months birthDate milestoneDate

        groupedSigns =
            Maybe.map (Date.diff Months birthDate) firstEncounterDateAfterMilestone
                |> groupedECDSigns ageMonths
    in
    ecdSignsFromGroupedSignsByAge ageWeeks ageMonths groupedSigns


groupedECDSigns : Int -> Maybe Int -> List (List ECDSign)
groupedECDSigns ageMonths ageMonthsAtLastAssessment =
    let
        ( from5Weeks, from13Weeks ) =
            Maybe.map
                (\ageMonthsLastAssessment ->
                    if ageMonthsLastAssessment >= 6 then
                        ( [], [] )

                    else
                        ( ecdSignsFrom5Weeks, ecdSignsFrom13Weeks )
                )
                ageMonthsAtLastAssessment
                |> Maybe.withDefault ( ecdSignsFrom5Weeks, ecdSignsFrom13Weeks )

        ecdSigns6To12MonthsByAge =
            if ageMonths > 12 then
                []

            else if ageMonths >= 9 then
                ecdSigns6To12MonthsMajors

            else
                ecdSigns6To12MonthsMinors ++ ecdSigns6To12MonthsMajors

        ecdSigns6To12Months =
            Maybe.map
                (\ageMonthsLastAssessment ->
                    if ageMonthsLastAssessment > 12 then
                        []

                    else if ageMonthsLastAssessment >= 9 then
                        ecdSigns6To12MonthsMajors

                    else if ageMonthsLastAssessment >= 6 then
                        ecdSigns6To12MonthsByAge

                    else
                        ecdSigns6To12MonthsMinors ++ ecdSigns6To12MonthsMajors
                )
                ageMonthsAtLastAssessment
                |> Maybe.withDefault (ecdSigns6To12MonthsMinors ++ ecdSigns6To12MonthsMajors)
    in
    [ from5Weeks
    , from13Weeks
    , ecdSigns6To12Months
    , ecdSignsFrom15Months
    , ecdSignsFrom18Months
    , ecdSignsFrom2Years
    , ecdSignsFrom3Years
    , ecdSignsFrom4Years
    ]


ecdSignsFrom5Weeks : List ECDSign
ecdSignsFrom5Weeks =
    [ FollowMothersEyes
    , MoveArmsAndLegs
    ]


ecdSignsFrom13Weeks : List ECDSign
ecdSignsFrom13Weeks =
    [ RaiseHandsUp
    , Smile
    , RollSideways
    ]


ecdSigns6To12MonthsMinors : List ECDSign
ecdSigns6To12MonthsMinors =
    [ BringHandsToMouth
    , HoldHeadWithoutSupport
    , HoldAndShakeToys
    , ReactToSuddenSounds
    , UseConsonantSounds
    ]


ecdSigns6To12MonthsMajors : List ECDSign
ecdSigns6To12MonthsMajors =
    [ RespondToSoundWithSound
    , TurnHeadWhenCalled
    , SitWithoutSupport
    , SmileBack
    , RollTummyToBack
    , ReachForToys
    ]


ecdSignsFrom15Months : List ECDSign
ecdSignsFrom15Months =
    [ UseSimpleGestures
    , StandOnTheirOwn
    , CopyDuringPlay
    , SayMamaDada
    , CanHoldSmallObjects
    ]


ecdSignsFrom18Months : List ECDSign
ecdSignsFrom18Months =
    [ LooksWhenPointedAt
    , UseSingleWords
    , WalkWithoutHelp
    , PlayPretend
    , PointToThingsOfInterest
    ]


ecdSignsFrom2Years : List ECDSign
ecdSignsFrom2Years =
    [ UseShortPhrases
    , InterestedInOtherChildren
    , FollowSimpleInstructions
    , KickBall
    , PointAtNamedObjects
    ]


ecdSignsFrom3Years : List ECDSign
ecdSignsFrom3Years =
    [ DressThemselves
    , WashHandsGoToToiled
    , KnowsColorsAndNumbers
    , UseMediumPhrases
    , PlayMakeBelieve
    ]


ecdSignsFrom4Years : List ECDSign
ecdSignsFrom4Years =
    [ FollowThreeStepInstructions
    , StandOnOneFootFiveSeconds
    , UseLongPhrases
    , ShareWithOtherChildren
    , CountToTen
    ]


ecdSignsFromGroupedSignsByAge : Int -> Int -> List (List ECDSign) -> List ECDSign
ecdSignsFromGroupedSignsByAge ageWeeks ageMonths groupedSigns =
    if ageWeeks < 5 then
        []

    else if ageWeeks < 13 then
        List.Extra.splitAt 1 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 6 then
        List.Extra.splitAt 2 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 15 then
        List.Extra.splitAt 3 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 18 then
        List.Extra.splitAt 4 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 24 then
        List.Extra.splitAt 5 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 36 then
        List.Extra.splitAt 6 groupedSigns
            |> Tuple.first
            |> List.concat

    else if ageMonths < 48 then
        List.Extra.splitAt 7 groupedSigns
            |> Tuple.first
            |> List.concat

    else
        List.concat groupedSigns


pediatricCareMilestoneToComparable : PediatricCareMilestone -> Int
pediatricCareMilestoneToComparable milestone =
    case milestone of
        Milestone6Weeks ->
            1

        Milestone14Weeks ->
            2

        Milestone6Months ->
            3

        Milestone9Months ->
            4

        Milestone12Months ->
            5

        Milestone15Months ->
            6

        Milestone18Months ->
            7

        Milestone2Years ->
            8

        Milestone3Years ->
            9

        Milestone4Years ->
            10


pediatricCareMilestones : List PediatricCareMilestone
pediatricCareMilestones =
    [ Milestone6Weeks
    , Milestone14Weeks
    , Milestone6Months
    , Milestone9Months
    , Milestone12Months
    , Milestone15Months
    , Milestone18Months
    , Milestone2Years
    , Milestone3Years
    , Milestone4Years
    ]
