module Pages.Scoreboard.Utils exposing (generateECDMilestonesWithStatus, generateFutureVaccinationsData)

import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (..)
import Backend.Scoreboard.Utils exposing (vaccineDoseToComparable)
import Date exposing (Unit(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Pages.Scoreboard.Model exposing (..)


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData :
    NominalDate
    -> VaccinationProgressDict
    -> List ( VaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData birthDate vaccinationProgress =
    let
        initialOpvAdministered =
            wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress
    in
    List.map
        (\vaccineType ->
            let
                nextVaccinationData =
                    case latestVaccinationDataForVaccine vaccinationProgress vaccineType of
                        Just ( lastDoseAdministered, lastDoseDate ) ->
                            nextVaccinationDataForVaccine vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered

                        Nothing ->
                            let
                                vaccinationDate =
                                    initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, VaccineDoseFirst )
                            in
                            Just ( VaccineDoseFirst, vaccinationDate )
            in
            -- Getting Nothing at nextVaccinationData indicates that
            -- vacination cycle is completed for this vaccine.
            ( vaccineType, nextVaccinationData )
        )
        allVaccineTypes


wasInitialOpvAdministeredByVaccinationProgress : NominalDate -> VaccinationProgressDict -> Bool
wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress =
    Dict.get VaccineOPV vaccinationProgress
        |> Maybe.andThen (Dict.get VaccineDoseFirst)
        |> Maybe.map
            (\adminstrationDate ->
                Date.diff Days birthDate adminstrationDate < 14
            )
        |> Maybe.withDefault False


latestVaccinationDataForVaccine : VaccinationProgressDict -> VaccineType -> Maybe ( VaccineDose, NominalDate )
latestVaccinationDataForVaccine vaccinationsData vaccineType =
    Dict.get vaccineType vaccinationsData
        |> Maybe.andThen
            (Dict.toList
                >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                >> List.reverse
                >> List.head
            )


nextVaccinationDataForVaccine : VaccineType -> Bool -> NominalDate -> VaccineDose -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered =
    if getLastDoseForVaccine initialOpvAdministered vaccineType == lastDoseAdministered then
        Nothing

    else
        getNextVaccineDose lastDoseAdministered
            |> Maybe.map
                (\dose ->
                    let
                        ( interval, unit ) =
                            getIntervalForVaccine vaccineType
                    in
                    ( dose, Date.add unit interval lastDoseDate )
                )


getLastDoseForVaccine : Bool -> VaccineType -> VaccineDose
getLastDoseForVaccine initialOpvAdministered vaccineType =
    case vaccineType of
        VaccineBCG ->
            VaccineDoseFirst

        VaccineOPV ->
            if initialOpvAdministered then
                VaccineDoseFourth

            else
                VaccineDoseThird

        VaccineDTP ->
            VaccineDoseThird

        VaccinePCV13 ->
            VaccineDoseThird

        VaccineRotarix ->
            VaccineDoseSecond

        VaccineIPV ->
            VaccineDoseFirst

        VaccineMR ->
            VaccineDoseSecond


getNextVaccineDose : VaccineDose -> Maybe VaccineDose
getNextVaccineDose dose =
    case dose of
        VaccineDoseFirst ->
            Just VaccineDoseSecond

        VaccineDoseSecond ->
            Just VaccineDoseThird

        VaccineDoseThird ->
            Just VaccineDoseFourth

        VaccineDoseFourth ->
            Just VaccineDoseFifth

        VaccineDoseFifth ->
            Nothing


getIntervalForVaccine : VaccineType -> ( Int, Unit )
getIntervalForVaccine vaccineType =
    case vaccineType of
        VaccineBCG ->
            ( 0, Days )

        VaccineOPV ->
            ( 4, Weeks )

        VaccineDTP ->
            ( 4, Weeks )

        VaccinePCV13 ->
            ( 4, Weeks )

        VaccineRotarix ->
            ( 4, Weeks )

        VaccineIPV ->
            ( 0, Days )

        VaccineMR ->
            ( 6, Months )


initialVaccinationDateByBirthDate : NominalDate -> Bool -> ( VaccineType, VaccineDose ) -> NominalDate
initialVaccinationDateByBirthDate birthDate initialOpvAdministered ( vaccineType, vaccineDose ) =
    let
        dosesInterval =
            vaccineDoseToComparable vaccineDose - 1

        ( interval, unit ) =
            getIntervalForVaccine vaccineType
    in
    case vaccineType of
        VaccineBCG ->
            birthDate

        VaccineOPV ->
            case vaccineDose of
                VaccineDoseFirst ->
                    birthDate

                _ ->
                    if initialOpvAdministered then
                        -- Second dose is given starting from age of 6 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit ((dosesInterval - 1) * interval)

                    else
                        -- Second dose is given starting from age of 10 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit (dosesInterval * interval)

        VaccineDTP ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccinePCV13 ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineRotarix ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineIPV ->
            Date.add Weeks 14 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineMR ->
            Date.add Weeks 36 birthDate
                |> Date.add unit (dosesInterval * interval)


allVaccineTypes : List VaccineType
allVaccineTypes =
    [ VaccineBCG
    , VaccineOPV
    , VaccineDTP
    , VaccinePCV13
    , VaccineRotarix
    , VaccineIPV
    , VaccineMR
    ]


generateECDMilestonesWithStatus :
    NominalDate
    -> NominalDate
    -- -> List ( WellChildEncounterId, WellChildEncounter )
    -- -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
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
