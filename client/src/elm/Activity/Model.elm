module Activity.Model exposing (..)


type alias ActivityIdentity =
    { name : String
    , icon : String
    }


type ActivityType
    = Child ChildActivityType
    | Mother MotherActivityType


type ChildActivityType
    = ChildPicture
    | Height
    | Weight
    | Muac
    | ProgressReport


type MotherActivityType
    = Aheza
    | Attendance
    | Education
    | FamilyPlanning
    | Hiv
    | MotherPicture
    | NutritionSigns


type alias ActivityReport =
    { activity : ActivityIdentity
    , remaining : Int
    }


activityList : List ActivityReport
activityList =
    [ { activity =
            { name = "Weight"
            , icon = "law"
            }
      , remaining = 3
      }
    , { activity =
            { name = "Height"
            , icon = "line chart"
            }
      , remaining = 2
      }
    , { activity =
            { name = "MUAC"
            , icon = "treatment"
            }
      , remaining = 4
      }
    , { activity =
            { name = "Education"
            , icon = "student"
            }
      , remaining = 5
      }
    , { activity =
            { name = "Nutrition signs"
            , icon = "heartbeat"
            }
      , remaining = 4
      }
    , { activity =
            { name = "Aheza"
            , icon = "food"
            }
      , remaining = 7
      }
    , { activity =
            { name = "Family planning"
            , icon = "users"
            }
      , remaining = 4
      }
    , { activity =
            { name = "HIV"
            , icon = "doctor"
            }
      , remaining = 4
      }
    , { activity =
            { name = "Attendance"
            , icon = "thumbs outline up"
            }
      , remaining = 0
      }
    , { activity =
            { name = "Take pictures"
            , icon = "photo"
            }
      , remaining = 0
      }
    , { activity =
            { name = "Progress reports"
            , icon = "bar chart"
            }
      , remaining = 0
      }
    ]
