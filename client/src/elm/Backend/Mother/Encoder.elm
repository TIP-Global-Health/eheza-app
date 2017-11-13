module Backend.Mother.Encoder exposing (..)

import Backend.Mother.Model exposing (..)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityId)


encodeMother : Mother -> List ( String, Value )
encodeMother mother =
    [ ( "label", string mother.name )
    , ( "avatar", string mother.image )
    , ( "children", list (List.map encodeEntityId mother.children) )
    , ( "date_birth", encodeYYYYMMDD mother.birthDate )
    ]
