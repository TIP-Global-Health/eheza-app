module Backend.ResilienceSurvey.Update exposing (update)

import Backend.Endpoints exposing (resilienceSurveyEndpoint)
import Backend.Entities exposing (..)
import Backend.ResilienceSurvey.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update currentDate msg model =
    case msg of
        CreateResilienceSurvey survey ->
            createResilienceSurvey currentDate survey model

        HandleCreatedResilienceSurvey data ->
            ( { model | createResilienceSurvey = data }
            , Cmd.none
            )


createResilienceSurvey : NominalDate -> ResilienceSurvey -> Model -> ( Model, Cmd Msg )
createResilienceSurvey currentDate survey model =
    ( { model | createResilienceSurvey = Loading }
    , sw.post resilienceSurveyEndpoint survey
        |> toCmd (RemoteData.fromResult >> HandleCreatedResilienceSurvey)
    )
