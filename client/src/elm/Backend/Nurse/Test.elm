module Backend.Nurse.Test exposing (all)

import AssocList as Dict
import Backend.Nurse.Model exposing (Nurse, Role(..))
import Backend.Nurse.Utils exposing (nurseAuthorizedForLocation)
import EverySet
import Expect
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)


{-| A minimal nurse carrying the given health-center and village assignments and
roles; the resilience fields are irrelevant to authorization.
-}
nurseWith : List String -> List String -> List Role -> Nurse
nurseWith healthCenterIds villageIds roles =
    { name = "Test Nurse"
    , healthCenters = EverySet.fromList (List.map toEntityUuid healthCenterIds)
    , villages = EverySet.fromList (List.map toEntityUuid villageIds)
    , roles = EverySet.fromList roles
    , email = Nothing
    , pinCode = "1234"
    , resilienceProgramEnabled = False
    , resilienceProgramStartDate = Nothing
    , resilienceRole = Nothing
    , resilienceBirthDate = Nothing
    , resilienceGender = Nothing
    , resilienceEducationLevel = Nothing
    , resilienceUbudehe = Nothing
    , resilienceMaritalStatus = Nothing
    , resilienceNextReminder = Nothing
    , resilienceMessages = Dict.empty
    }


nurseAuthorizedForLocationTest : Test
nurseAuthorizedForLocationTest =
    describe "nurseAuthorizedForLocation"
        [ describe "CHW (authorized per village)"
            [ test "authorized when the selected village is one of theirs" <|
                \_ ->
                    nurseAuthorizedForLocation (Just (toEntityUuid "v1")) (Just (toEntityUuid "hc1")) (nurseWith [ "hc1" ] [ "v1" ] [ RoleCHW ])
                        |> Expect.equal True
            , test "NOT authorized after reassignment to a village they no longer hold - even though their parent health center still matches" <|
                -- The bug: App.View gated on health center only, so this returned
                -- True and the CHW kept working under the revoked village.
                \_ ->
                    nurseAuthorizedForLocation (Just (toEntityUuid "v2")) (Just (toEntityUuid "hc1")) (nurseWith [ "hc1" ] [ "v1" ] [ RoleCHW ])
                        |> Expect.equal False
            , test "NOT authorized when no village is selected" <|
                \_ ->
                    nurseAuthorizedForLocation Nothing (Just (toEntityUuid "hc1")) (nurseWith [ "hc1" ] [ "v1" ] [ RoleCHW ])
                        |> Expect.equal False
            ]
        , describe "non-CHW nurse (authorized per health center)"
            [ test "authorized when the selected health center is one of theirs" <|
                \_ ->
                    nurseAuthorizedForLocation Nothing (Just (toEntityUuid "hc1")) (nurseWith [ "hc1" ] [] [ RoleNurse ])
                        |> Expect.equal True
            , test "NOT authorized when the selected health center is not theirs" <|
                \_ ->
                    nurseAuthorizedForLocation Nothing (Just (toEntityUuid "hc2")) (nurseWith [ "hc1" ] [] [ RoleNurse ])
                        |> Expect.equal False
            , test "NOT authorized when no health center is selected" <|
                \_ ->
                    nurseAuthorizedForLocation Nothing Nothing (nurseWith [ "hc1" ] [] [ RoleNurse ])
                        |> Expect.equal False
            , test "a nurse's village membership does not authorize a health-center selection" <|
                \_ ->
                    nurseAuthorizedForLocation (Just (toEntityUuid "v1")) (Just (toEntityUuid "hc2")) (nurseWith [ "hc1" ] [ "v1" ] [ RoleNurse ])
                        |> Expect.equal False
            ]
        ]


all : Test
all =
    describe "Backend.Nurse" [ nurseAuthorizedForLocationTest ]
