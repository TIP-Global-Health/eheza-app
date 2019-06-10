module Pages.PrenatalEncounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    }


type Msg
    = SetActivePage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }
