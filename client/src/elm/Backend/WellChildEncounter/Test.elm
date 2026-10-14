module Backend.WellChildEncounter.Test exposing (all)

import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), Msg(..), WellChildEncounterType(..), ecdMilestoneWarnings, emptyModel, emptyWellChildEncounter, hasWarningFrom, headCircumferenceWarnings)
import Backend.WellChildEncounter.Update exposing (replaceWarnings, update)
import Date
import EverySet
import Expect
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Time


replaceWarningsTest : Test
replaceWarningsTest =
    let
        -- Lists the result in a fixed order, since sets do not compare by
        -- content.
        replace group replacement warnings =
            let
                result =
                    replaceWarnings group replacement (EverySet.fromList warnings)
            in
            List.filter (\warning -> EverySet.member warning result)
                (ecdMilestoneWarnings ++ headCircumferenceWarnings ++ [ NoEncounterWarnings ])
    in
    describe "replaceWarnings"
        [ test "clearing removes a cephaly warning saved earlier" <|
            \_ ->
                replace headCircumferenceWarnings [] [ WarningHeadCircumferenceMacrocephaly ]
                    |> Expect.equal [ NoEncounterWarnings ]
        , test "clearing keeps the ECD warnings" <|
            \_ ->
                replace headCircumferenceWarnings [] [ WarningECDMilestoneBehind, WarningHeadCircumferenceMicrocephaly ]
                    |> Expect.equal [ WarningECDMilestoneBehind ]
        , test "clearing removes the not taken mark" <|
            \_ ->
                replace headCircumferenceWarnings [] [ NoECDMilstoneWarning, NoHeadCircumferenceWarning ]
                    |> Expect.equal [ NoECDMilstoneWarning ]
        , test "a new cephaly warning replaces the earlier one" <|
            \_ ->
                replace headCircumferenceWarnings [ WarningHeadCircumferenceMicrocephaly ] [ WarningHeadCircumferenceMacrocephaly ]
                    |> Expect.equal [ WarningHeadCircumferenceMicrocephaly ]
        , test "an ECD warning replaces only the ECD group" <|
            \_ ->
                replace ecdMilestoneWarnings [ NoECDMilstoneWarning ] [ NoEncounterWarnings, WarningECDMilestoneBehind, WarningHeadCircumferenceMacrocephaly ]
                    |> Expect.equal [ NoECDMilstoneWarning, WarningHeadCircumferenceMacrocephaly ]
        ]


clearHeadCircumferenceWarningsTest : Test
clearHeadCircumferenceWarningsTest =
    let
        -- Whether clearing saves the encounter, read from the request state.
        saves warnings =
            let
                currentDate =
                    Date.fromCalendarDate 2026 Time.Oct 14

                encounter =
                    emptyWellChildEncounter (toEntityUuid "participant") currentDate PediatricCare Nothing

                ( updatedModel, _, _ ) =
                    update currentDate
                        Nothing
                        Nothing
                        (toEntityUuid "encounter")
                        (Just { encounter | encounterWarnings = EverySet.fromList warnings })
                        ClearHeadCircumferenceWarnings
                        emptyModel
            in
            updatedModel.editWellChildEncounter == Loading
    in
    describe "ClearHeadCircumferenceWarnings"
        [ test "saves an encounter holding a cephaly warning" <|
            \_ ->
                saves [ WarningECDMilestoneBehind, WarningHeadCircumferenceMacrocephaly ]
                    |> Expect.equal True
        , test "leaves an encounter with no head circumference warning unsaved" <|
            \_ ->
                saves [ WarningECDMilestoneBehind ]
                    |> Expect.equal False
        ]


hasWarningFromTest : Test
hasWarningFromTest =
    let
        warnings =
            EverySet.fromList [ WarningECDMilestoneBehind, NoEncounterWarnings ]
    in
    describe "hasWarningFrom"
        [ test "finds a warning of the group" <|
            \_ ->
                hasWarningFrom ecdMilestoneWarnings warnings
                    |> Expect.equal True
        , test "finds none from the other group" <|
            \_ ->
                hasWarningFrom headCircumferenceWarnings warnings
                    |> Expect.equal False
        ]


all : Test
all =
    describe "Backend.WellChildEncounter"
        [ clearHeadCircumferenceWarningsTest
        , hasWarningFromTest
        , replaceWarningsTest
        ]
