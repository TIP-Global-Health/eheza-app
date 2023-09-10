module Backend.Measurement.Test exposing (all)

import Backend.Measurement.Model exposing (ColorAlertIndication(..), MuacInCm(..))
import Backend.Measurement.Utils exposing (muacIndication)
import Expect
import Test exposing (Test, describe, test)


muacIndicationTest : Test
muacIndicationTest =
    describe "MuacIndication tests"
        [ describe "muacIndication"
            [ test "red" <|
                \_ ->
                    muacIndication (MuacInCm 11.5)
                        |> Expect.equal ColorAlertRed
            , test "yellow1" <|
                \_ ->
                    muacIndication (MuacInCm 11.6)
                        |> Expect.equal ColorAlertYellow
            , test "yellow2" <|
                \_ ->
                    muacIndication (MuacInCm 12.5)
                        |> Expect.equal ColorAlertYellow
            , test "green" <|
                \_ ->
                    muacIndication (MuacInCm 12.6)
                        |> Expect.equal ColorAlertGreen
            ]
        ]


{-| This one tests that we can decode the 'edits' from local storage in the
way we encoded them as of 2018-11-16. This is important, because when we
deploy a new version of the app, we'll still need to deal with sessions in
progress. So, we can't fail to decode the previous contents of local storage.
-}
decodeMeasurementEditsTest : Test
decodeMeasurementEditsTest =
    describe "decodeMeasurementEdits"
        [ test "back-compat" <|
            \_ ->
                Expect.pass

        -- TODO: Consider deployment strategy, and whether we need back-compat
        {-
           \_ ->
               let
                   -- Some sample JSON we need to continue to be able to decode
                   json =
                       """{"mothers":{"10":{"family_planning":{"tag":"unedited"},"checked_in":true},"17":{"family_planning":{"tag":"unedited"},"checked_in":true},"18":{"family_planning":{"tag":"unedited"},"checked_in":true},"20":{"family_planning":{"tag":"created","value":{"mother":20,"session":164,"date_measured":"2018-11-16","family_planning_signs":["condoms","pill"]}},"checked_in":true},"23":{"family_planning":{"tag":"unedited"},"checked_in":true}},"children":{"41":{"height":{"tag":"created","value":{"child":41,"session":164,"date_measured":"2018-11-16","height":27}},"muac":{"tag":"unedited"},"nutrition":{"tag":"created","value":{"child":41,"session":164,"date_measured":"2018-11-16","nutrition_signs":["edema"]}},"photo":{"tag":"unedited"},"weight":{"tag":"created","value":{"child":41,"session":164,"date_measured":"2018-11-16","weight":28}}}},"closed":false}"""
               in
               decodeString decodeMeasurementEdits json
                   |> (\result ->
                           case result of
                               Ok _ ->
                                   Expect.pass

                               Err msg ->
                                   Expect.fail msg
                      )
        -}
        ]


all : Test
all =
    describe "Measurement data tests"
        [ muacIndicationTest
        , decodeMeasurementEditsTest
        ]
