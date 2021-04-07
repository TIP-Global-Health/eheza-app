module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (generateNutritionFollowUps)


fetch : NominalDate -> HealthCenterId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId db =
    let
        nutritionFollowUps =
            generateNutritionFollowUps currentDate healthCenterId db

        -- @todo: Can be a single message?
        fetchPatientsMsgs =
            Dict.keys nutritionFollowUps
                |> List.map FetchPerson
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    ]
        ++ fetchPatientsMsgs
