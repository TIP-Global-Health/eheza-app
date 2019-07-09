module Backend.PrenatalParticipant.Encoder exposing (encodePrenatalParticipant)

import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodePrenatalParticipant : PrenatalParticipant -> Value
encodePrenatalParticipant data =
    object
        [ ( "person", encodeEntityUuid data.person )
        , ( "expected"
          , object
                [ ( "value", encodeYYYYMMDD data.start )
                , ( "value2", maybe encodeYYYYMMDD data.end )
                ]
          )
        ]
