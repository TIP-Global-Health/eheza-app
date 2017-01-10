module Pages.Activities.Model exposing (Activity, Model)


type alias Model =
    List Activity


type alias Activity =
    { name : String
    , icon : String
    , remaining : Int
    }


activityList : Model
activityList =
    [ { name = "Weight"
      , icon = "law"
      , remaining = 3
      }
    , { name = "Height"
      , icon = "line chart"
      , remaining = 2
      }
    , { name = "MUAC"
      , icon = "treatment"
      , remaining = 4
      }
    , { name = "Education"
      , icon = "student"
      , remaining = 5
      }
    , { name = "Nutrition signs"
      , icon = "heartbeat"
      , remaining = 4
      }
    , { name = "Aheza"
      , icon = "food"
      , remaining = 7
      }
    , { name = "Family planning"
      , icon = "users"
      , remaining = 4
      }
    , { name = "HIV"
      , icon = "doctor"
      , remaining = 4
      }
    , { name = "Attendance"
      , icon = "thumbs outline up"
      , remaining = 0
      }
    , { name = "Take pictures"
      , icon = "photo"
      , remaining = 0
      }
    , { name = "Progress reports"
      , icon = "bar chart"
      , remaining = 0
      }
    ]
