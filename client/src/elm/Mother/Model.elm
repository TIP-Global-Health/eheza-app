module Mother.Model exposing (..)


type alias ChildId =
    String


type alias MotherId =
    String


type alias Mother =
    { name : String
    , icon : String
    , children : List ChildId
    }
