module Backend.PmtctParticipant.Encoder exposing (encodePmtctParticipant)

import Backend.PmtctParticipant.Model exposing (AdultActivities(..), PmtctParticipant)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodePmtctParticipant : PmtctParticipant -> List ( String, Value )
encodePmtctParticipant data =
    [ ( "person", encodeEntityUuid data.child )
    , ( "adult", encodeEntityUuid data.adult )
    , ( "adult_activities", encodeAdultActivities data.adultActivities )
    , ( "expected"
      , object
            [ ( "value", encodeYYYYMMDD data.start )
            , ( "value2", maybe encodeYYYYMMDD data.end )
            ]
      )
    , ( "clinic", encodeEntityUuid data.clinic )
    , ( "deleted", bool data.deleted )
    , ( "type", string "pmtct_participant" )
    ]


encodeAdultActivities : AdultActivities -> Value
encodeAdultActivities data =
    case data of
        CaregiverActivities ->
            string "caregiver"

        MotherActivities ->
            string "mother"
