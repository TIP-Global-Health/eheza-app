module Backend.Menu.Model exposing (..)

import Json.Encode exposing (Value)


type Site
    = SiteRwanda
    | SiteBurundi
    | SiteUnknown


type alias MenuData =
    { site : Site
    }


type Msg
    = SetData Value
