module Pages.Participants.Model exposing (..)

import App.PageType exposing (Page(..))


type alias Model =
    { selectedTab : Tab
    }


type Msg
    = SetRedirectPage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 122
    , height = 122
    }
