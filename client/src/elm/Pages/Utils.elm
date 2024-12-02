module Pages.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (HealthCenterId, PersonId)
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , ImageUrl(..)
        , MedicationDistributionSign(..)
        , MedicationNonAdministrationSign(..)
        )
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.Session.Model exposing (OfflineSession)
import Backend.Session.Utils exposing (getChildren)
import Backend.Utils exposing (reportToWhatsAppEnabled)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, toLastDayOfMonth)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import List.Zipper as Zipper
import Maybe.Extra exposing (isJust, or, unwrap)
import Restful.Endpoint exposing (fromEntityUuid)
import Round
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


viewReportLink : Language -> TranslationId -> msg -> Html msg
viewReportLink language labelTransId action =
    div
        [ class "report-wrapper"
        , onClick action
        ]
        [ div [ class "icon-progress-report" ] []
        , div [ class "report-text" ]
            [ div [ class "report-label" ] [ text <| translate language labelTransId ]
            , div [ class "report-link" ] [ text <| translate language Translate.View ]
            ]
        ]


viewPersonDetails : Language -> NominalDate -> Person -> Maybe TranslationId -> List (Html msg)
viewPersonDetails language currentDate person maybeDiagnosisTranslationId =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate ->
                            let
                                renderAgeFunc =
                                    if isAboveAgeOf2Years currentDate person then
                                        renderAgeYearsMonths

                                    else
                                        renderAgeMonthsDays
                            in
                            renderAgeFunc language birthDate currentDate
                        )
                )
    in
    [ div [ class "ui image" ]
        [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
    , div [ class "content person-details" ]
        [ h2 [ class "ui header" ]
            [ text person.name ]
        , maybeAge
            |> Maybe.map
                (\age ->
                    p [ class "age-wrapper" ]
                        [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ":" ]
                        , span [] [ text age ]
                        ]
                )
            |> Maybe.withDefault emptyNode
        , maybeDiagnosisTranslationId
            |> Maybe.map
                (\diagnosis ->
                    div
                        [ classList
                            [ ( "diagnosis-wrapper", True )
                            , ( "covid-19", diagnosis == Translate.AcuteIllnessDiagnosis DiagnosisCovid19Suspect )
                            ]
                        ]
                        [ div [ class "label upper" ] [ text <| translate language Translate.Diagnosis ++ ":" ]
                        , div [ class "diagnosis" ] [ text <| translate language diagnosis ]
                        ]
                )
            |> Maybe.withDefault emptyNode
        ]
    ]


isAboveAgeOf2Years : NominalDate -> Person -> Bool
isAboveAgeOf2Years currentDate person =
    ageInYears currentDate person
        |> Maybe.map (\age -> age >= 2)
        |> Maybe.withDefault False


viewPersonDetailsExtended : Language -> NominalDate -> Person -> List (Html any)
viewPersonDetailsExtended language currentDate person =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, ageEntry ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\ageYears -> viewEntry Translate.AgeWord (Translate.YearsOld ageYears |> translate language))
                    |> Maybe.withDefault emptyNode
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate ->
                            let
                                renderAgeFunc =
                                    if isAboveAgeOf2Years currentDate person then
                                        renderAgeYearsMonths

                                    else
                                        renderAgeMonthsDays
                            in
                            viewEntry Translate.AgeWord (renderAgeFunc language birthDate currentDate)
                        )
                    |> Maybe.withDefault emptyNode
                )

        dateOfBirthEntry =
            Maybe.map
                (\birthDate ->
                    viewEntry Translate.DateOfBirth (formatDDMMYYYY birthDate)
                )
                person.birthDate
                |> Maybe.withDefault emptyNode

        genderEntry =
            viewEntry Translate.GenderLabel (translate language <| Translate.Gender person.gender)

        villageEntry =
            Maybe.map (viewEntry Translate.Village) person.village
                |> Maybe.withDefault emptyNode

        viewEntry labelTransId content =
            p []
                [ span [ class "label" ] [ text <| translate language labelTransId ++ ": " ]
                , span [] [ text content ]
                ]
    in
    [ div [ class "ui image" ]
        [ thumbnailImage thumbnailClass person.avatarUrl person.name 140 140 ]
    , div [ class "details" ]
        [ h2 [ class "ui header" ]
            [ text person.name ]
        , ageEntry
        , dateOfBirthEntry
        , genderEntry
        , villageEntry
        ]
    ]


calculatePercentage : Int -> Int -> Float
calculatePercentage now before =
    -- Avoid dividing by zero and getting "NaN", just return 0.
    -- Besides, if the total is 0, then we don't need to calculate anything here.
    if before == 0 && now == 0 then
        0

    else if before == 0 && now > 0 then
        100

    else
        let
            diff =
                abs (now - before)
        in
        if now > before then
            (toFloat diff / toFloat before) * 100

        else
            (toFloat diff / toFloat before) * -100


filterDependentNoResultsMessage : Language -> String -> TranslationId -> String
filterDependentNoResultsMessage language filter message =
    if String.isEmpty filter then
        translate language message

    else
        translate language Translate.NoMatchesFound


matchFilter : String -> String -> Bool
matchFilter filter filteredValue =
    if String.isEmpty filter then
        True

    else
        filteredValue
            |> String.toLower
            |> String.contains filter


matchMotherAndHerChildren : String -> OfflineSession -> PersonId -> Person -> Bool
matchMotherAndHerChildren filter offlineSession motherId mother =
    let
        motherContainsFilter =
            matchFilter filter mother.name

        -- A function, rather than value, to preserve the
        -- short-circuiting benefits of the `||` below.
        childrenContainsFilter _ =
            getChildren motherId offlineSession
                |> List.any
                    (\( _, child ) ->
                        matchFilter filter child.name
                    )
    in
    motherContainsFilter || childrenContainsFilter ()


viewNameFilter : Language -> String -> (String -> msg) -> Html msg
viewNameFilter language filterInput setFilterMsg =
    viewCustomNameFilter language filterInput setFilterMsg Translate.FilterByName


viewCustomNameFilter : Language -> String -> (String -> msg) -> TranslationId -> Html msg
viewCustomNameFilter language filterInput setFilterMsg placeholderTransId =
    div [ class "ui action input small" ]
        [ input
            [ placeholder <| translate language placeholderTransId
            , type_ "text"
            , onInput setFilterMsg
            , value filterInput
            ]
            []
        , button
            [ classList
                [ ( "ui button primary", True )
                , ( "disabled", String.isEmpty <| normalizeFilter filterInput )
                ]
            , onClick <| setFilterMsg ""
            ]
            [ text <| translate language Translate.Clear ]
        ]


normalizeFilter : String -> String
normalizeFilter =
    String.toLower >> String.trim


viewLabel : Language -> TranslationId -> Html any
viewLabel language translationId =
    viewCustomLabel language translationId ":" "label"


viewQuestionLabel : Language -> TranslationId -> Html any
viewQuestionLabel language translationId =
    viewCustomLabel language translationId "?" "label"


viewCustomLabel : Language -> TranslationId -> String -> String -> Html any
viewCustomLabel language translationId suffix class_ =
    div [ class class_ ] [ text <| (translate language translationId ++ suffix) ]


getCurrentReasonForMedicationNonAdministration :
    (AdministrationNote -> MedicationNonAdministrationSign)
    -> { f | nonAdministrationSigns : Maybe (EverySet MedicationNonAdministrationSign) }
    -> Maybe AdministrationNote
getCurrentReasonForMedicationNonAdministration reasonToSignFunc form =
    let
        nonAdministrationSigns =
            form.nonAdministrationSigns |> Maybe.withDefault EverySet.empty
    in
    [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientDeclined, NonAdministrationPatientUnableToAfford, NonAdministrationOther ]
        |> List.filterMap
            (\reason ->
                if EverySet.member (reasonToSignFunc reason) nonAdministrationSigns then
                    Just reason

                else
                    Nothing
            )
        |> List.head


nonAdministrationReasonToSign : MedicationDistributionSign -> AdministrationNote -> MedicationNonAdministrationSign
nonAdministrationReasonToSign sign reason =
    case sign of
        Amoxicillin ->
            MedicationAmoxicillin reason

        Coartem ->
            MedicationCoartem reason

        ORS ->
            MedicationORS reason

        Zinc ->
            MedicationZinc reason

        Paracetamol ->
            MedicationParacetamol reason

        Mebendezole ->
            MedicationMebendezole reason

        Tenofovir ->
            MedicationTenofovir reason

        Lamivudine ->
            MedicationLamivudine reason

        Dolutegravir ->
            MedicationDolutegravir reason

        TDF3TC ->
            MedicationTDF3TC reason

        Iron ->
            MedicationIron reason

        FolicAcid ->
            MedicationFolicAcid reason

        Ceftriaxone ->
            MedicationCeftriaxone reason

        Azithromycin ->
            MedicationAzithromycin reason

        Metronidazole ->
            MedicationMetronidazole reason

        VitaminA ->
            MedicationVitaminA reason

        -- Below are not in use, but we specify them explicitly to make
        -- sure that compile arets if we forget to address new
        -- MedicationDistributionSign, when added.
        Albendazole ->
            NoMedicationNonAdministrationSigns

        LemonJuiceOrHoney ->
            NoMedicationNonAdministrationSigns

        NoMedicationDistributionSigns ->
            NoMedicationNonAdministrationSigns

        NoMedicationDistributionSignsInitialPhase ->
            NoMedicationNonAdministrationSigns

        NoMedicationDistributionSignsRecurrentPhase ->
            NoMedicationNonAdministrationSigns


viewMonthSelector : Language -> NominalDate -> Int -> Int -> (Int -> msg) -> Html msg
viewMonthSelector language selectedDate monthGap maxGap changeMonthGapMsg =
    let
        monthNumber =
            Date.monthNumber selectedDate

        month =
            Date.numberToMonth monthNumber

        year =
            Date.year selectedDate
    in
    div [ class "month-selector" ]
        [ span
            [ classList
                [ ( "icon-back", True )
                , ( "hidden", monthGap == maxGap )
                ]
            , onClick <| changeMonthGapMsg 1
            ]
            []
        , span [ class "label" ]
            [ text <| translate language (Translate.ResolveMonth False month) ++ " " ++ String.fromInt year ]
        , span
            [ classList
                [ ( "icon-back rotate-180", True )
                , ( "hidden", monthGap == 0 )
                ]
            , onClick <| changeMonthGapMsg -1
            ]
            []
        ]


{-| If current month is selected, returns current date.
If any of previous months is selected, returns last day of selected months.
-}
resolveSelectedDateForMonthSelector : NominalDate -> Int -> NominalDate
resolveSelectedDateForMonthSelector currentDate monthGap =
    if monthGap == 0 then
        currentDate

    else
        Date.add Date.Months (-1 * monthGap) currentDate
            |> toLastDayOfMonth



-- Inputs


viewSearchForm : Language -> String -> TranslationId -> (String -> msg) -> Html msg
viewSearchForm language inputValue placeholderTransId setInputMsg =
    div [ class "ui search form" ]
        [ viewTextInput language inputValue setInputMsg (Just placeholderTransId) (Just "search-input") ]


viewTextInput : Language -> String -> (String -> msg) -> Maybe TranslationId -> Maybe String -> Html msg
viewTextInput language inputValue setInputMsg placeholderTransId inputClass =
    let
        attributes =
            inputClassAttribute
                ++ placeholderAttribute
                ++ [ type_ "text"
                   , onInput setInputMsg
                   , value inputValue
                   , autofocus True
                   ]

        inputClassAttribute =
            Maybe.map (class >> List.singleton) inputClass
                |> Maybe.withDefault []

        placeholderAttribute =
            Maybe.map (translate language >> placeholder >> List.singleton)
                placeholderTransId
                |> Maybe.withDefault []
    in
    input attributes []


viewBoolInput :
    Language
    -> Maybe Bool
    -> (Bool -> msg)
    -> String
    -> Maybe ( TranslationId, TranslationId )
    -> Html msg
viewBoolInput language currentValue setMsg inputClass optionsTranslationIds =
    let
        ( yesTransId, noTransId ) =
            Maybe.withDefault ( Translate.Yes, Translate.No ) optionsTranslationIds

        inputWidth =
            if isJust optionsTranslationIds then
                "eight"

            else
                "four"
    in
    viewCustomBoolInput language currentValue setMsg inputClass ( yesTransId, noTransId ) inputWidth False


viewBoolInputReverted :
    Language
    -> Maybe Bool
    -> (Bool -> msg)
    -> String
    -> Maybe ( TranslationId, TranslationId )
    -> Html msg
viewBoolInputReverted language currentValue setMsg inputClass optionsTranslationIds =
    let
        ( yesTransId, noTransId ) =
            Maybe.withDefault ( Translate.Yes, Translate.No ) optionsTranslationIds

        inputWidth =
            if isJust optionsTranslationIds then
                "eight"

            else
                "four"
    in
    viewCustomBoolInput language currentValue setMsg inputClass ( yesTransId, noTransId ) inputWidth True


viewCustomBoolInput :
    Language
    -> Maybe Bool
    -> (Bool -> msg)
    -> String
    -> ( TranslationId, TranslationId )
    -> String
    -> Bool
    -> Html msg
viewCustomBoolInput language currentValue setMsg inputClass ( yesTransId, noTransId ) inputWidth isReverted =
    let
        inputs =
            if isReverted then
                [ viewInput False, viewInput True ]

            else
                [ viewInput True, viewInput False ]

        viewInput value =
            let
                isChecked =
                    currentValue == Just value

                transId =
                    if value then
                        yesTransId

                    else
                        noTransId
            in
            div [ class <| inputWidth ++ " wide column" ]
                [ input
                    [ type_ "radio"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    , onCheck (always (setMsg value))
                    ]
                    []
                , label [ onClick <| setMsg value ]
                    [ text <| translate language transId ]
                ]
    in
    div [ class <| "form-input yes-no " ++ inputClass ]
        [ div [ class "ui grid" ] inputs ]


viewCheckBoxSelectInput : Language -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxSelectInput language leftOptions rightOptions currentValue setMsg translateFunc =
    let
        viewOptionFunc option =
            label []
                [ translateFunc option |> translate language |> text ]
    in
    viewCheckBoxSelectCustomInput language leftOptions rightOptions currentValue setMsg viewOptionFunc


viewCheckBoxSelectInputWithRecommendation : Language -> List a -> List a -> a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxSelectInputWithRecommendation language leftOptions rightOptions recommendedOption currentValue setMsg translateFunc =
    let
        viewOptionFunc option =
            if option == recommendedOption then
                label [ class "recommendation" ]
                    [ div [] [ translateFunc option |> translate language |> text ]
                    , div [ class "marker" ] [ text <| "(" ++ translate language Translate.Recommended ++ ")" ]
                    ]

            else
                label []
                    [ translateFunc option |> translate language |> text ]
    in
    viewCheckBoxSelectCustomInput language leftOptions rightOptions currentValue setMsg viewOptionFunc


viewCheckBoxSelectCustomInput : Language -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> Html msg) -> Html msg
viewCheckBoxSelectCustomInput language leftOptions rightOptions currentValue setMsg viewOptionFunc =
    let
        checkedOptions =
            Maybe.map List.singleton currentValue
                |> Maybe.withDefault []
    in
    viewCheckBoxMultipleSelectCustomInput language leftOptions rightOptions checkedOptions Nothing setMsg viewOptionFunc


viewCheckBoxMultipleSelectInput : Language -> List a -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions noneOption setMsg translateFunc =
    let
        viewOptionFunc option =
            label [] [ translateFunc option |> translate language |> text ]
    in
    viewCheckBoxMultipleSelectCustomInput language leftOptions rightOptions checkedOptions noneOption setMsg viewOptionFunc


viewCheckBoxMultipleSelectCustomInput : Language -> List a -> List a -> List a -> Maybe a -> (a -> msg) -> (a -> Html msg) -> Html msg
viewCheckBoxMultipleSelectCustomInput language leftOptions rightOptions checkedOptions noneOption setMsg viewOptionFunc =
    let
        noneSection =
            noneOption
                |> unwrap
                    []
                    (\option ->
                        [ div [ class "ui divider" ] []
                        , viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc option
                        ]
                    )

        ( leftOptionsClass, rightOptionsSection ) =
            if List.isEmpty rightOptions then
                ( "sixteen", emptyNode )

            else
                ( "eight"
                , rightOptions
                    |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc)
                    |> div [ class "eight wide column" ]
                )
    in
    div [ class "checkbox-select-input" ] <|
        div [ class "ui grid" ]
            [ leftOptions
                |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc)
                |> div [ class <| leftOptionsClass ++ " wide column" ]
            , rightOptionsSection
            ]
            :: noneSection


viewCheckBoxMultipleSelectSectionsInput : Language -> List ( TranslationId, List a ) -> List a -> (a -> msg) -> (a -> TranslationId) -> Html msg
viewCheckBoxMultipleSelectSectionsInput language sections checkedOptions setMsg translateFunc =
    let
        viewSection ( labelTransId, options ) =
            let
                viewOptionFunc option =
                    label [] [ translateFunc option |> translate language |> text ]
            in
            div [ class "section" ] <|
                viewLabel language labelTransId
                    :: List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc) options
    in
    div [ class "checkbox-select-input" ]
        [ div [ class "ui grid" ]
            [ List.map viewSection sections
                |> div [ class "sixteen wide column" ]
            ]
        ]


viewCheckBoxSelectInputItem : Language -> List a -> (a -> msg) -> (a -> Html msg) -> a -> Html msg
viewCheckBoxSelectInputItem language checkedOptions setMsg viewOptionFunc option =
    let
        isChecked =
            List.member option checkedOptions
    in
    div
        [ class "ui checkbox activity"
        , onClick <| setMsg option
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , classList [ ( "checked", isChecked ) ]
            ]
            []
        , viewOptionFunc option
        ]


viewNumberInput : Language -> Maybe Int -> (String -> msg) -> String -> Html msg
viewNumberInput language maybeCurrentValue setMsg inputClass =
    let
        currentValue =
            Maybe.map String.fromInt maybeCurrentValue
                |> Maybe.withDefault ""

        inputAttrs =
            [ type_ "number"
            , Html.Attributes.min "0"
            , onInput setMsg
            , value currentValue
            ]
    in
    div [ class <| "form-input number " ++ inputClass ]
        [ input inputAttrs [] ]


viewMeasurementInput : Language -> Maybe Float -> (String -> msg) -> String -> TranslationId -> Html msg
viewMeasurementInput language maybeCurrentValue setMsg inputClass unitTranslationId =
    let
        currentValue =
            maybeCurrentValue
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault ""

        inputAttrs =
            [ type_ "number"
            , Html.Attributes.min "0"
            , onInput setMsg
            , value currentValue
            ]
    in
    div [ class <| "form-input measurement " ++ inputClass ]
        [ input inputAttrs []
        , div [ class "unit" ]
            [ text <| translate language unitTranslationId ]
        ]


viewPreviousMeasurement : Language -> Maybe Float -> TranslationId -> Html any
viewPreviousMeasurement language maybePreviousValue unitTranslationId =
    let
        message =
            maybePreviousValue
                |> unwrap
                    (translate language Translate.PreviousMeasurementNotFound)
                    (\previousValue ->
                        (previousValue
                            |> Translate.PreviousFloatMeasurement
                            |> translate language
                        )
                            ++ " "
                            ++ translate language unitTranslationId
                    )
    in
    div [ class "previous-value" ] [ text message ]


viewCheckBoxValueInput : Language -> ( List a, a ) -> Dict a Int -> (a -> msg) -> (a -> String -> msg) -> (a -> TranslationId) -> List (Html msg)
viewCheckBoxValueInput language ( signs, none ) data toggleMsg setMsg translateFunc =
    let
        items =
            List.map (viewCheckBoxValueInputItem language data toggleMsg setMsg translateFunc) signs

        noneItem =
            [ viewCheckBoxValueInputNone language data toggleMsg translateFunc none ]
    in
    items ++ noneItem


viewCheckBoxValueInputItem : Language -> Dict a Int -> (a -> msg) -> (a -> String -> msg) -> (a -> TranslationId) -> a -> Html msg
viewCheckBoxValueInputItem language data toggleMsg setMsg translateFunc sign =
    let
        currentValue =
            Dict.get sign data

        isChecked =
            isJust currentValue

        periodSection =
            if isChecked then
                let
                    periodInput =
                        viewCustomSelectListInput currentValue
                            (List.range 1 14)
                            String.fromInt
                            (setMsg sign)
                            String.fromInt
                            "form-input period"
                            False
                in
                [ div [ class "three wide column" ] [ periodInput ]
                , div [ class "four wide column" ]
                    [ div [ class "days-present" ] [ text <| translate language Translate.DaysPresent ] ]
                ]

            else
                []
    in
    div [ class "ui grid" ] <|
        div [ class "eight wide column" ]
            [ div
                [ class "ui checkbox activity"
                , onClick <| toggleMsg sign
                ]
                [ input
                    [ type_ "checkbox"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    ]
                    []
                , label []
                    [ text <| translate language (translateFunc sign) ]
                ]
            ]
            :: periodSection


viewCheckBoxValueInputNone : Language -> Dict a Int -> (a -> msg) -> (a -> TranslationId) -> a -> Html msg
viewCheckBoxValueInputNone language data setMsg translateFunc noneSign =
    let
        currentValue =
            Dict.get noneSign data

        isChecked =
            isJust currentValue

        action =
            if isChecked then
                []

            else
                [ onClick <| setMsg noneSign ]
    in
    div [ class "ui grid" ]
        [ div
            [ class "seven wide column" ]
            [ div (class "ui checkbox activity" :: action)
                [ input
                    [ type_ "checkbox"
                    , checked isChecked
                    , classList [ ( "checked", isChecked ) ]
                    ]
                    []
                , label []
                    [ text <| translate language (translateFunc noneSign) ]
                ]
            ]
        ]


setMultiSelectInputValue : (f -> Maybe (List s)) -> (Maybe (List s) -> f) -> s -> s -> f -> f
setMultiSelectInputValue getSignsFunc setSignsFunc noValueIndicator value form =
    case getSignsFunc form of
        Just signs ->
            if List.member value signs then
                let
                    updatedSigns =
                        if List.length signs == 1 then
                            Nothing

                        else
                            signs |> List.filter ((/=) value) |> Just
                in
                setSignsFunc updatedSigns

            else if value == noValueIndicator then
                setSignsFunc (Just [ value ])

            else
                let
                    updatedSigns =
                        if signs == [ noValueIndicator ] then
                            Just [ value ]

                        else
                            Just (value :: signs)
                in
                setSignsFunc updatedSigns

        Nothing ->
            setSignsFunc (Just [ value ])


viewSelectListInput :
    Language
    -> Maybe a
    -> List a
    -> (a -> String)
    -> (String -> msg)
    -> (a -> TranslationId)
    -> String
    -> Html msg
viewSelectListInput language currentValue options toStringFunc setMsg transId inputClass =
    viewCustomSelectListInput currentValue
        options
        toStringFunc
        setMsg
        (transId >> translate language)
        ("form-input " ++ inputClass)
        True


viewCustomSelectListInput :
    Maybe a
    -> List a
    -> (a -> String)
    -> (String -> msg)
    -> (a -> String)
    -> String
    -> Bool
    -> Html msg
viewCustomSelectListInput currentValue options toStringFunc setMsg transFunc inputClass withEmptyOption =
    let
        emptyOption =
            if withEmptyOption then
                emptySelectOption (currentValue == Nothing)

            else
                emptyNode
    in
    emptyOption
        :: List.map
            (\option_ ->
                option
                    [ value (toStringFunc option_)
                    , selected (currentValue == Just option_)
                    ]
                    [ text <| transFunc option_ ]
            )
            options
        |> select
            [ onInput setMsg
            , class inputClass
            ]


emptySelectOption : Bool -> Html any
emptySelectOption isSelected =
    option
        [ value ""
        , selected isSelected
        ]
        [ text "" ]


viewEndEncounterDialog : Language -> TranslationId -> TranslationId -> msg -> msg -> Html msg
viewEndEncounterDialog language heading message confirmAction cancelAction =
    div [ class "ui tiny active modal" ]
        [ div [ class "header" ]
            [ text <| translate language heading ]
        , div
            [ class "content" ]
            [ p [] [ text <| translate language message ]
            ]
        , div
            [ class "actions" ]
            [ div [ class "two ui buttons" ]
                [ button
                    [ class "ui fluid button"
                    , onClick cancelAction
                    ]
                    [ text <| translate language Translate.Cancel ]
                , button
                    [ class "ui primary fluid button"
                    , onClick confirmAction
                    ]
                    [ text <| translate language Translate.Continue ]
                ]
            ]
        ]


viewSkipNCDADialog : Language -> msg -> msg -> Html msg
viewSkipNCDADialog language confirmAction cancelAction =
    div [ class "ui tiny active modal" ]
        [ div [ class "content" ]
            [ p [] [ text <| translate language Translate.SkipNCDADialogQuestion ] ]
        , div
            [ class "actions" ]
            [ div [ class "two ui buttons" ]
                [ button
                    [ class "ui primary fluid button"
                    , onClick confirmAction
                    ]
                    [ text <| translate language Translate.SkipNCDADialogConfirm ]
                , button
                    [ class "ui fluid button"
                    , onClick cancelAction
                    ]
                    [ text <| translate language Translate.SkipNCDADialogReject ]
                ]
            ]
        ]


viewStartEncounterButton : Language -> msg -> Html msg
viewStartEncounterButton language action =
    viewEncounterActionButton language Translate.StartEncounter "primary" True action


viewEndEncounterButton : Language -> Bool -> msg -> Html msg
viewEndEncounterButton language =
    viewEndEncounterButtonCustomColor language "primary"


viewEndEncounterButtonCustomColor : Language -> String -> Bool -> msg -> Html msg
viewEndEncounterButtonCustomColor language buttonColor =
    viewEncounterActionButton language Translate.EndEncounter buttonColor


viewEncounterActionButton : Language -> TranslationId -> String -> Bool -> msg -> Html msg
viewEncounterActionButton language label buttonColor allowAction action =
    let
        attributes =
            if allowAction then
                [ class <| "ui fluid button " ++ buttonColor
                , onClick action
                ]

            else
                [ class <| "ui fluid button disabled " ++ buttonColor ]
    in
    div [ class "actions" ]
        [ button attributes
            [ text <| translate language label ]
        ]


viewEndEncounterMenuForProgressReport : Language -> EverySet SiteFeature -> Bool -> (Bool -> msg) -> msg -> Html msg
viewEndEncounterMenuForProgressReport language features allowEndEncounter setDialogStateMsg setReportToWhatsAppDialogStateMsg =
    let
        ( actionsClass, endEncounterButtonColor, reportToWhatsAppButton ) =
            if reportToWhatsAppEnabled features then
                ( "actions two"
                , "velvet"
                , button
                    [ class "ui fluid primary button"
                    , onClick setReportToWhatsAppDialogStateMsg
                    ]
                    [ text <| translate language Translate.ReportToWhatsApp ]
                )

            else
                ( "actions"
                , "primary"
                , emptyNode
                )

        attributes =
            if allowEndEncounter then
                [ class <| "ui fluid button " ++ endEncounterButtonColor
                , onClick <| setDialogStateMsg True
                ]

            else
                [ class <| "ui fluid button disabled " ++ endEncounterButtonColor ]
    in
    div [ class actionsClass ]
        [ button attributes
            [ text <| translate language Translate.EndEncounter ]
        , reportToWhatsAppButton
        ]


viewRedAlertForSelect : List a -> List a -> Html any
viewRedAlertForSelect actual normal =
    viewAlertForSelect "red" actual normal


viewYellowAlertForSelect : List a -> List a -> Html any
viewYellowAlertForSelect actual normal =
    viewAlertForSelect "yellow" actual normal


viewAlertForSelect : String -> List a -> List a -> Html any
viewAlertForSelect color actual normal =
    if
        List.isEmpty actual
            || List.all
                (\item ->
                    List.member item normal
                )
                actual
    then
        emptyNode

    else
        div [ class <| "alert " ++ color ]
            [ viewAlert color ]


viewRedAlertForBool : Maybe Bool -> Bool -> Html any
viewRedAlertForBool actual normal =
    viewRedAlertForSelect
        (actual |> Maybe.map List.singleton |> Maybe.withDefault [])
        [ normal ]


{-| The idea here is that we get lists for red alert conditions, and yellow
alert conditions. If any of red conditions matches, we present red alert.
If any of yellow conditions matches, we present yellow alert.
Otherwise, no alret is needed.

Note that conditions are list of lists, so all conditions in inner list
need to match, for a condition in outer list to match.
We need this for range conditions. For example, number between 5 and 8.

-}
viewConditionalAlert : Maybe a -> List (List (a -> Bool)) -> List (List (a -> Bool)) -> Html any
viewConditionalAlert maybeActual redConditions yellowConditions =
    maybeActual
        |> Maybe.map
            (\actual ->
                if
                    List.any
                        (\conditions ->
                            List.all
                                (\condition ->
                                    condition actual
                                )
                                conditions
                        )
                        redConditions
                then
                    viewAlert "red"

                else if
                    List.any
                        (\conditions ->
                            List.all (\condition -> condition actual) conditions
                        )
                        yellowConditions
                then
                    viewAlert "yellow"

                else
                    emptyNode
            )
        |> Maybe.withDefault emptyNode


viewAlert : String -> Html any
viewAlert color =
    let
        icon =
            "assets/images/alert-" ++ color ++ ".png"
    in
    img [ src icon ] []


viewInstructionsLabel : String -> Html any -> Html any
viewInstructionsLabel iconClass message =
    div [ class "header icon-label" ] <|
        [ i [ class iconClass ] []
        , message
        ]


taskCompleted : Maybe a -> Int
taskCompleted maybe =
    if isJust maybe then
        1

    else
        0


taskCompletedWithException : Maybe a -> a -> Int
taskCompletedWithException maybe exception =
    case maybe of
        Just value ->
            if value == exception then
                0

            else
                1

        Nothing ->
            0


taskAllCompleted : List (Maybe a) -> Int
taskAllCompleted list =
    if List.all isJust list then
        1

    else
        0


taskAnyCompleted : List (Maybe a) -> Int
taskAnyCompleted list =
    if List.any isJust list then
        1

    else
        0


ifEverySetEmpty : a -> EverySet a -> EverySet a
ifEverySetEmpty value set =
    if EverySet.isEmpty set then
        EverySet.singleton value

    else
        set


ifTrue : a -> Bool -> EverySet a
ifTrue value condition =
    if condition then
        EverySet.singleton value

    else
        EverySet.empty


ifFalse : a -> Bool -> EverySet a
ifFalse value condition =
    ifTrue value (not condition)


ifNullableTrue : a -> Maybe Bool -> Maybe (EverySet a)
ifNullableTrue value maybeCondition =
    Maybe.map (ifTrue value >> Just) maybeCondition
        |> Maybe.withDefault (Just EverySet.empty)


valueConsideringIsDirtyField : Bool -> Maybe a -> a -> Maybe a
valueConsideringIsDirtyField isDirty formVariant valueVariant =
    maybeValueConsideringIsDirtyField isDirty formVariant (Just valueVariant)


maybeValueConsideringIsDirtyField : Bool -> Maybe a -> Maybe a -> Maybe a
maybeValueConsideringIsDirtyField isDirty formVariant maybeValueVariant =
    if isDirty then
        formVariant

    else
        or formVariant maybeValueVariant


{-| Show a photo thumbnail.
-}
viewPhotoThumb : String -> Html any
viewPhotoThumb url =
    div []
        [ img
            [ src url
            , class "ui small image orientation"
            ]
            []
        ]


viewPhotoThumbFromImageUrl : ImageUrl -> Html any
viewPhotoThumbFromImageUrl (ImageUrl url) =
    viewPhotoThumb url


isTaskCompleted : Dict t ( Int, Int ) -> t -> Bool
isTaskCompleted dict task =
    Dict.get task dict
        |> Maybe.map (\( completed, total ) -> completed == total)
        |> Maybe.withDefault False


maybeToBoolTask : Maybe a -> Maybe Bool
maybeToBoolTask maybe =
    if isJust maybe then
        Just True

    else
        Nothing


resolveTasksCompletedFromTotal : List (Maybe Bool) -> ( Int, Int )
resolveTasksCompletedFromTotal tasks =
    ( Maybe.Extra.values tasks
        |> List.length
    , List.length tasks
    )


resolveNextTask : t -> Dict t ( Int, Int ) -> List t -> Maybe t
resolveNextTask activeTask completedFromTotalDict allTasks =
    List.filter
        (\task ->
            (task /= activeTask)
                && (not <| isTaskCompleted completedFromTotalDict task)
        )
        allTasks
        |> List.head


tasksBarId : String
tasksBarId =
    "tasks-bar"


viewSaveAction : Language -> msg -> Bool -> Html msg
viewSaveAction language saveMsg disabled =
    viewCustomAction language saveMsg disabled Translate.Save


viewCustomAction : Language -> msg -> Bool -> Translate.TranslationId -> Html msg
viewCustomAction language saveMsg disabled label =
    div [ class "actions" ]
        [ customButton language (not disabled) saveMsg label ]


saveButton : Language -> Bool -> msg -> Html msg
saveButton language active msg =
    customButton language active msg Translate.Save


customButton : Language -> Bool -> msg -> Translate.TranslationId -> Html msg
customButton language active msg label =
    let
        attributes =
            classList
                [ ( "ui fluid primary button", True )
                , ( "active", active )
                , ( "disabled", not active )
                ]
                :: (if active then
                        [ onClick msg ]

                    else
                        []
                   )
    in
    button attributes
        [ text <| translate language label ]


viewTasksCount : Language -> Int -> Int -> Html any
viewTasksCount language tasksCompleted totalTasks =
    div [ class "tasks-count" ]
        [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]


insertIntoSet : a -> Maybe (EverySet a) -> Maybe (EverySet a)
insertIntoSet value set =
    Maybe.map (EverySet.insert value) set
        |> Maybe.withDefault (EverySet.singleton value)
        |> Just


customPopup : Language -> Bool -> TranslationId -> String -> ( Html msg, Html msg, msg ) -> Html msg
customPopup language showWarning actionLabel extraClass ( topMessage, bottomMessage, action ) =
    div [ class <| "ui active modal " ++ extraClass ]
        [ div [ class "content" ] <|
            [ div [ class "popup-heading-wrapper" ]
                [ img [ src "assets/images/exclamation-red.png" ] []
                , div [ class "popup-heading" ] [ text <| translate language Translate.Warning ++ "!" ]
                ]
                |> showIf showWarning
            , div [ class "popup-title" ]
                [ topMessage
                , bottomMessage
                ]
            ]
        , div
            [ class "actions" ]
            [ button
                [ class "ui primary fluid button"
                , onClick action
                ]
                [ text <| translate language actionLabel ]
            ]
        ]


unique : List a -> List a
unique =
    EverySet.fromList >> EverySet.toList


viewBySyncStatus : Language -> HealthCenterId -> SyncManager.Model.SyncInfoAuthorityZipper -> Html msg -> Html msg
viewBySyncStatus language healthCenterId syncInfoAuthorities contentForView =
    let
        selectedHealthCenterSyncInfo =
            syncInfoAuthorities
                |> Maybe.andThen
                    (Zipper.toList >> List.Extra.find (\authorityInfo -> authorityInfo.uuid == fromEntityUuid healthCenterId))

        showWarningMessage header message =
            div [ class "ui message warning" ]
                [ div [ class "header" ] [ text <| translate language header ]
                , text <| translate language message
                ]
    in
    selectedHealthCenterSyncInfo
        |> Maybe.map
            (\syncInfo ->
                case syncInfo.status of
                    SyncManager.Model.NotAvailable ->
                        showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync

                    SyncManager.Model.Downloading ->
                        -- Our goal is to limit disturance to operation during
                        -- Download phase for HC, only at case it's mandatory, which
                        -- is initial HC sync (since downloaded data may not be
                        -- complete, which may cause errors and unstable behavior).
                        -- Subsequent syncs will download (much) less than 1500
                        -- entities, so if we bellow that, it's safe to allow operations.
                        -- Since download is done with batches of 500, it will take only
                        -- 3 batches to complete download, a matter or few seconds, so,
                        -- we should not get to operations with incomplete data situation.
                        if syncInfo.remainingToDownload > 1500 then
                            showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCDownloading

                        else
                            contentForView

                    _ ->
                        contentForView
            )
        |> Maybe.withDefault
            (showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync)


setMuacValueForSite : Site -> String -> Maybe Float
setMuacValueForSite site s =
    case site of
        SiteBurundi ->
            -- At Burundi, value is entered as mm, but we need to store it
            -- as cm. Therefore, we multiply by 0.1.
            String.toFloat s
                |> Maybe.map ((*) 0.1 >> Round.roundNum 1)

        _ ->
            String.toFloat s


resolveActiveTask : List t -> Maybe t -> Maybe t
resolveActiveTask options selected =
    Maybe.map
        (\task ->
            if List.member task options then
                Just task

            else
                List.head options
        )
        selected
        |> Maybe.withDefault (List.head options)


concatInputsAndTasksSections : List ( List (Html msg), List (Maybe Bool) ) -> ( List (Html msg), List (Maybe Bool) )
concatInputsAndTasksSections sections =
    ( List.map Tuple.first sections |> List.concat
    , List.map Tuple.second sections |> List.concat
    )
