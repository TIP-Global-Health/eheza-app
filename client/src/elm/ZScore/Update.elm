module ZScore.Update exposing (update)

import Http exposing (Error, expectJson)
import HttpBuilder exposing (get, toTask, withExpect)
import RemoteData exposing (RemoteData(..), WebData, isNotAsked)
import Task
import Update.Extra exposing (sequence)
import Utils.AllDict as AllDict exposing (AllDict)
import Utils.NominalDate exposing (Days(..), Months(..))
import ZScore.Decoder exposing (..)
import ZScore.Model exposing (..)


type alias MonthsAndDays a =
    { days : a
    , months : a
    }


bmiForAgePaths : MonthsAndDays String
bmiForAgePaths =
    { days = "assets/z-score/bmianthro.json"
    , months = "assets/z-score/bfawho2007.json"
    }


lengthHeightForAgePaths : MonthsAndDays String
lengthHeightForAgePaths =
    { days = "assets/z-score/lenanthro.json"
    , months = "assets/z-score/hfawho2007.json"
    }


lengthHeightForAge5to19Paths : MonthsAndDays String
lengthHeightForAge5to19Paths =
    { days = "assets/z-score/lenanthro5to19.json"
    , months = "assets/z-score/hfawho2007.json"
    }


weightForAgePaths : MonthsAndDays String
weightForAgePaths =
    { days = "assets/z-score/weianthro.json"
    , months = "assets/z-score/wfawho2007.json"
    }


weightForHeightPath : String
weightForHeightPath =
    "assets/z-score/wfhanthro.json"


weightForLengthPath : String
weightForLengthPath =
    "assets/z-score/wflanthro.json"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchAllTables ->
            ( model, Cmd.none )
                |> sequence update
                    [ FetchBmiForAgeTables
                    , FetchLengthHeightForAgeTables
                    , FetchWeightForAgeTables
                    , FetchWeightForHeightTables
                    , FetchWeightForLengthTables
                    ]

        FetchBmiForAgeTables ->
            if isNotAsked model.bmiForAge then
                ( { model | bmiForAge = Loading }
                , fetchForAge bmiForAgePaths BMI HandleBmiForAgeTables
                )

            else
                ( model, Cmd.none )

        FetchLengthHeightForAgeTables ->
            if isNotAsked model.lengthHeightForAge then
                ( { model | lengthHeightForAge = Loading }
                , fetchForAge lengthHeightForAgePaths Centimetres HandleLengthHeightForAgeTables
                )

            else
                ( model, Cmd.none )

        FetchLengthHeightForAge5to19Tables ->
            if isNotAsked model.lengthHeightForAge5to19 then
                ( { model | lengthHeightForAge5to19 = Loading }
                , fetchForAge lengthHeightForAge5to19Paths Centimetres HandleLengthHeightForAge5to19Tables
                )

            else
                ( model, Cmd.none )

        FetchWeightForAgeTables ->
            if isNotAsked model.weightForAge then
                ( { model | weightForAge = Loading }
                , fetchForAge weightForAgePaths Kilograms HandleWeightForAgeTables
                )

            else
                ( model, Cmd.none )

        FetchWeightForHeightTables ->
            if isNotAsked model.weightForHeight then
                ( { model | weightForHeight = Loading }
                , fetchForHeight weightForHeightPath HandleWeightForHeightTables
                )

            else
                ( model, Cmd.none )

        FetchWeightForLengthTables ->
            if isNotAsked model.weightForLength then
                ( { model | weightForLength = Loading }
                , fetchForLength weightForLengthPath HandleWeightForLengthTables
                )

            else
                ( model, Cmd.none )

        HandleBmiForAgeTables data ->
            ( { model | bmiForAge = data }
            , Cmd.none
            )

        HandleLengthHeightForAgeTables data ->
            ( { model | lengthHeightForAge = data }
            , Cmd.none
            )

        HandleLengthHeightForAge5to19Tables data ->
            ( { model | lengthHeightForAge5to19 = data }
            , Cmd.none
            )

        HandleWeightForAgeTables data ->
            ( { model | weightForAge = data }
            , Cmd.none
            )

        HandleWeightForHeightTables data ->
            ( { model | weightForHeight = data }
            , Cmd.none
            )

        HandleWeightForLengthTables data ->
            ( { model | weightForLength = data }
            , Cmd.none
            )


fetchForAge : MonthsAndDays String -> (Float -> a) -> (WebData (MaleAndFemale (ByDaysAndMonths a)) -> Msg) -> Cmd Msg
fetchForAge paths wrapper tagger =
    let
        daysTask =
            get paths.days
                |> withExpect (expectJson (decodeForAge Days (\(Days x) -> x) wrapper))
                |> toTask

        monthsTask =
            get paths.months
                |> withExpect (expectJson (decodeForAge Months (\(Months x) -> x) wrapper))
                |> toTask
    in
    daysTask
        |> Task.andThen
            (\daysResult ->
                Task.map
                    (\monthsResult ->
                        { male =
                            { byDay = daysResult.male
                            , byMonth = monthsResult.male
                            }
                        , female =
                            { byDay = daysResult.female
                            , byMonth = monthsResult.female
                            }
                        }
                    )
                    monthsTask
            )
        |> RemoteData.asCmd
        |> Cmd.map tagger


{-| Fetch JSON data keyed by a "length" field which is in centimetres
-}
fetchForLength : String -> (WebData (MaleAndFemale (AllDict Length (ZScoreEntry Kilograms) Int)) -> Msg) -> Cmd Msg
fetchForLength path tagCmd =
    get path
        |> withExpect (expectJson (decodeForCentimetres "length" Length (\(Length x) -> x)))
        |> toTask
        |> RemoteData.asCmd
        |> Cmd.map tagCmd


{-| Fetch JSON data keyed by a "height" field which is in centimetres
-}
fetchForHeight : String -> (WebData (MaleAndFemale (AllDict Height (ZScoreEntry Kilograms) Int)) -> Msg) -> Cmd Msg
fetchForHeight path tagCmd =
    get path
        |> withExpect (expectJson (decodeForCentimetres "height" Height (\(Height x) -> x)))
        |> toTask
        |> RemoteData.asCmd
        |> Cmd.map tagCmd
