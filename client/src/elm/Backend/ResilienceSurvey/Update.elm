module Backend.ResilienceSurvey.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (resilienceSurveyEndpoint)
import Backend.ResilienceSurvey.Model exposing (..)
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        CreateResilienceSurvey survey ->
            createResilienceSurvey currentDate survey model

        HandleCreatedResilienceSurvey data ->
            ( { model | createResilienceSurvey = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


createResilienceSurvey : NominalDate -> ResilienceSurvey -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
createResilienceSurvey currentDate survey model =
    ( { model | createResilienceSurvey = Loading }
    , sw.post resilienceSurveyEndpoint survey
        |> toCmd (RemoteData.fromResult >> HandleCreatedResilienceSurvey)
    , []
    )
