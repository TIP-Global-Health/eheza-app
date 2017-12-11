module ZScore.Update exposing (update)

import Http exposing (Error, expectJson)
import HttpBuilder exposing (get, withExpect, toTask)
import IntDict exposing (IntDict)
import RemoteData exposing (WebData, RemoteData(..), isNotAsked)
import Update.Extra exposing (sequence)
import ZScore.Decoder exposing (..)
import ZScore.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchAll ->
            ( model, Cmd.none )
                |> sequence update
                    [ FetchHeightForAgeBoys
                    , FetchHeightForAgeGirls
                    , FetchWeightForAgeBoys
                    , FetchWeightForAgeGirls
                    , FetchWeightForHeightBoys
                    , FetchWeightForHeightGirls
                    ]

        FetchHeightForAgeBoys ->
            if isNotAsked model.heightForAgeBoys then
                ( { model | heightForAgeBoys = Loading }
                , fetchForAge "assets/z-score/lhfa_boys_z_exp.json" HandleHeightForAgeBoys
                )
            else
                ( model, Cmd.none )

        FetchHeightForAgeGirls ->
            if isNotAsked model.heightForAgeGirls then
                ( { model | heightForAgeGirls = Loading }
                , fetchForAge "assets/z-score/lhfa_girls_z_exp.json" HandleHeightForAgeGirls
                )
            else
                ( model, Cmd.none )

        FetchWeightForAgeBoys ->
            if isNotAsked model.weightForAgeBoys then
                ( { model | weightForAgeBoys = Loading }
                , fetchForAge "assets/z-score/wfa_boys_z_exp.json" HandleWeightForAgeBoys
                )
            else
                ( model, Cmd.none )

        FetchWeightForAgeGirls ->
            if isNotAsked model.weightForAgeGirls then
                ( { model | weightForAgeGirls = Loading }
                , fetchForAge "assets/z-score/wfa_girls_z_exp.json" HandleWeightForAgeGirls
                )
            else
                ( model, Cmd.none )

        FetchWeightForHeightBoys ->
            if isNotAsked model.weightForHeightBoys then
                ( { model | weightForHeightBoys = Loading }
                , fetchForHeight "assets/z-score/wfh_boys_z_exp.json" HandleWeightForHeightBoys
                )
            else
                ( model, Cmd.none )

        FetchWeightForHeightGirls ->
            if isNotAsked model.weightForHeightGirls then
                ( { model | weightForHeightGirls = Loading }
                , fetchForHeight "assets/z-score/wfh_girls_z_exp.json" HandleWeightForHeightGirls
                )
            else
                ( model, Cmd.none )

        HandleHeightForAgeBoys data ->
            ( { model | heightForAgeBoys = data }
            , Cmd.none
            )

        HandleHeightForAgeGirls data ->
            ( { model | heightForAgeGirls = data }
            , Cmd.none
            )

        HandleWeightForAgeBoys data ->
            ( { model | weightForAgeBoys = data }
            , Cmd.none
            )

        HandleWeightForAgeGirls data ->
            ( { model | weightForAgeGirls = data }
            , Cmd.none
            )

        HandleWeightForHeightBoys data ->
            ( { model | weightForHeightBoys = data }
            , Cmd.none
            )

        HandleWeightForHeightGirls data ->
            ( { model | weightForHeightGirls = data }
            , Cmd.none
            )


{-| Fetch JSON data keyed by an "Day" field which is integer days.
-}
fetchForAge : String -> (WebData (IntDict ZScoreEntry) -> Msg) -> Cmd Msg
fetchForAge path tag =
    get path
        |> withExpect (expectJson decodeZScoreEntriesByDay)
        |> toTask
        |> RemoteData.asCmd
        |> Cmd.map tag


{-| Fetch JSON data keyed by an "Height" field which is float cm,
which we convert to integer millimetres.
-}
fetchForHeight : String -> (WebData (IntDict ZScoreEntry) -> Msg) -> Cmd Msg
fetchForHeight path tag =
    get path
        |> withExpect (expectJson decodeZScoreEntriesByHeight)
        |> toTask
        |> RemoteData.asCmd
        |> Cmd.map tag
