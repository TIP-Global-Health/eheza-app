module Pages.NCD.RecurrentEncounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }


type Msg
    = SetActivePage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports
