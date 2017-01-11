module Child.Model exposing (..)


type alias ChildId =
    String


type alias MotherId =
    String


type alias Child =
    { name : String
    , icon : String
    , motherId : MotherId
    }
