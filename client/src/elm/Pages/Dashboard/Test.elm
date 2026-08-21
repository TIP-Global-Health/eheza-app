module Pages.Dashboard.Test exposing (all)

import AssocList as Dict
import Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw)
import Backend.Dashboard.Encoder exposing (encodeDashboardStatsRaw)
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, DashboardStatsRaw, NutritionStatus(..), NutritionValue)
import Backend.Measurement.Model exposing (Gender(..))
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode
import Json.Encode
import Pages.Dashboard.Utils exposing (caseManagementMergeDuplicates)
import Test exposing (Test, describe, test)
import Time



-- FIXTURES


emptyCaseNutrition : CaseNutrition
emptyCaseNutrition =
    { stunting = Dict.empty
    , underweight = Dict.empty
    , wasting = Dict.empty
    , muac = Dict.empty
    , nutritionSigns = Dict.empty
    }


nutritionValue : NutritionValue
nutritionValue =
    { class = Moderate, value = "1.0" }


{-| A case-management record for person `identifier`, holding a single stunting
measurement for the given `month`.
-}
caseWithStuntingMonth : Int -> Int -> CaseManagement
caseWithStuntingMonth identifier month =
    { identifier = identifier
    , birthDate = Date.fromCalendarDate 2018 Time.Jan 1
    , gender = Female
    , nutrition = { emptyCaseNutrition | stunting = Dict.singleton month nutritionValue }
    }


caseManagementMergeDuplicatesTest : Test
caseManagementMergeDuplicatesTest =
    describe "caseManagementMergeDuplicates"
        [ test "merging two records for the SAME person keeps BOTH persons' nutrition months" <|
            \_ ->
                -- The same child enrolled across programs appears as two records
                -- sharing one identifier, each carrying different months of
                -- nutrition data. The merge must combine them, not discard one.
                let
                    merged =
                        caseManagementMergeDuplicates
                            [ caseWithStuntingMonth 1 1
                            , caseWithStuntingMonth 1 2
                            ]
                in
                case merged of
                    [ person ] ->
                        Dict.keys person.nutrition.stunting
                            |> List.sort
                            |> Expect.equal [ 1, 2 ]

                    _ ->
                        Expect.fail "expected the two same-identifier records to merge into exactly one"
        , test "records with DIFFERENT identifiers are not merged" <|
            \_ ->
                caseManagementMergeDuplicates
                    [ caseWithStuntingMonth 1 1
                    , caseWithStuntingMonth 2 3
                    ]
                    |> List.length
                    |> Expect.equal 2
        ]


generatedOn : NominalDate
generatedOn =
    Date.fromCalendarDate 2026 Time.Jul 15


statsWithGenerationDate : DashboardStatsRaw
statsWithGenerationDate =
    { caseManagement = { thisYear = Dict.empty, lastYear = Dict.empty }
    , childrenBeneficiaries = Dict.empty
    , completedPrograms = []
    , familyPlanning = []
    , missedSessions = []
    , totalEncounters = { global = Dict.empty, villages = Dict.empty }
    , acuteIllnessData = []
    , prenatalData = []
    , ncdData = []
    , pmtctData = []
    , spvData = []
    , childScoreboardData = []
    , nutritionIndividualData = []
    , nutritionGroupData = []
    , groupEducationData = Dict.empty
    , villagesWithResidents = Dict.empty
    , patientsDetails = Dict.empty
    , timestamp = "15-07-2026, 09:49"
    , statsGeneratedDate = Just generatedOn
    , cacheHash = "hash"
    }


statsStorageRoundTripTest : Test
statsStorageRoundTripTest =
    describe "encodeDashboardStatsRaw"
        [ test "keeps the generation date, which the monthly figures are read against" <|
            \_ ->
                -- Downloaded statistics are re-encoded through this function
                -- before being stored, and decoded again on the next start. A
                -- field the encoder drops is gone from then on.
                statsWithGenerationDate
                    |> encodeDashboardStatsRaw
                    |> Json.Encode.object
                    |> Json.Decode.decodeValue decodeDashboardStatsRaw
                    |> Result.map .statsGeneratedDate
                    |> Expect.equal (Ok (Just generatedOn))
        ]


all : Test
all =
    describe "Pages.Dashboard.Utils"
        [ caseManagementMergeDuplicatesTest
        , statsStorageRoundTripTest
        ]
