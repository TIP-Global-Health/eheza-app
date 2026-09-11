module Pages.PatientRecord.Test exposing (all)

import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (MsgIndexedDb(..), emptyModelIndexedDb)
import Date
import Expect
import Pages.PatientRecord.Fetch
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures
import Time


{-| An adult's patient record opens on the Acute Illness pane, which lists one
entry per acute-illness participant and reads the encounters out of the cache.
Only the explicit fetch puts them there, so the pane renders empty unless the
page asks for them.
-}
all : Test
all =
    let
        currentDate =
            Date.fromCalendarDate 2026 Time.Jul 14

        personId =
            toEntityUuid "person"

        acuteIllnessParticipantId =
            toEntityUuid "acute-illness-participant"

        prenatalParticipantId =
            toEntityUuid "prenatal-participant"

        db =
            { emptyModelIndexedDb
              -- The shared fixture is born 1985, so an adult at currentDate.
                | people = Dict.singleton personId (RemoteData.Success TestFixtures.testPerson)
                , individualParticipantsByPerson =
                    Dict.singleton personId
                        (RemoteData.Success <|
                            Dict.fromList
                                [ ( acuteIllnessParticipantId
                                  , TestFixtures.testParticipant currentDate AcuteIllnessEncounter
                                  )
                                , ( prenatalParticipantId
                                  , TestFixtures.testParticipant currentDate AntenatalEncounter
                                  )
                                ]
                        )
            }

        msgs =
            Pages.PatientRecord.Fetch.fetch currentDate personId db
    in
    describe "Pages.PatientRecord.Fetch"
        [ test "an adult's record asks for the acute-illness encounters" <|
            \_ ->
                List.member (FetchAcuteIllnessEncountersForParticipants [ acuteIllnessParticipantId ]) msgs
                    |> Expect.equal True
        , test "and still asks for the antenatal encounters it asked for before" <|
            \_ ->
                List.member (FetchPrenatalEncountersForParticipants [ prenatalParticipantId ]) msgs
                    |> Expect.equal True
        ]
