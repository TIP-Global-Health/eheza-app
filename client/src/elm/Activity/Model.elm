module Activity.Model exposing (..)

import Date exposing (Date)


type ActivityType
    = Child ChildActivityType
    | Mother MotherActivityType


type ChildActivityType
    = ChildPicture
    | Height
    | Muac
    | ProgressReport
    | Weight


type MotherActivityType
    = Aheza
    | Attendance
    | Education
    | FamilyPlanning
    | Hiv
    | MotherPicture
    | NutritionSigns


type alias ChildActivityDates =
    { childPicture : Maybe Date
    , height : Maybe Date
    , muac : Maybe Date
    , progressReport : Maybe Date
    , weight : Maybe Date
    }


type alias MotherActivityDates =
    { aheza : Maybe Date
    , attendance : Maybe Date
    , education : Maybe Date
    , familyPlanning : Maybe Date
    , hiv : Maybe Date
    , motherPicture : Maybe Date
    , nutritionSigns : Maybe Date
    }


type alias ActivityIdentity =
    { name : String
    , icon : String
    }


type alias ActivityListItem =
    { activity : ActivityIdentity
    , remaining : Int
    }


activityList : List ActivityListItem
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
