module Pages.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , ImageUrl(..)
        , MedicationDistributionSign(..)
        , MedicationDistributionValue
        , MedicationNonAdministrationSign(..)
        )
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.Session.Model exposing (OfflineSession)
import Backend.Session.Utils exposing (getChildren)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, or, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Time exposing (Month(..))
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

        isAboveAgeOf2Years =
            ageInYears currentDate person
                |> Maybe.map (\age -> age >= 2)
                |> Maybe.withDefault False

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                let
                    renderAgeFunc =
                        if isAboveAgeOf2Years then
                            renderAgeYearsMonths

                        else
                            renderAgeMonthsDays
                in
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeFunc language birthDate currentDate)
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


viewPersonDetailsExtended : Language -> NominalDate -> Person -> List (Html any)
viewPersonDetailsExtended language currentDate person =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        isAboveAgeOf2Years =
            ageInYears currentDate person
                |> Maybe.map (\age -> age >= 2)
                |> Maybe.withDefault False

        ( thumbnailClass, ageEntry ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\ageYears -> viewEntry Translate.AgeWord (Translate.YearsOld ageYears |> translate language))
                    |> Maybe.withDefault emptyNode
                )

            else
                let
                    renderAgeFunc =
                        if isAboveAgeOf2Years then
                            renderAgeYearsMonths

                        else
                            renderAgeMonthsDays
                in
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> viewEntry Translate.AgeWord (renderAgeFunc language birthDate currentDate))
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


normalizeFilter : String -> String
normalizeFilter filterInput =
    filterInput
        |> String.toLower
        |> String.trim


viewNameFilter : Language -> String -> (String -> msg) -> Html msg
viewNameFilter language filterInput setFilterMsg =
    div
        [ class "ui action input small" ]
        [ input
            [ placeholder <| translate language Translate.FilterByName
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


resolveSelectedDateForMonthSelector : NominalDate -> Int -> NominalDate
resolveSelectedDateForMonthSelector currentDate monthGap =
    Date.add Date.Months (-1 * monthGap) currentDate



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
            optionsTranslationIds |> Maybe.withDefault ( Translate.Yes, Translate.No )

        inputWidth =
            if isJust optionsTranslationIds then
                "eight"

            else
                "four"
    in
    viewCustomBoolInput language currentValue setMsg inputClass ( yesTransId, noTransId ) inputWidth


viewCustomBoolInput :
    Language
    -> Maybe Bool
    -> (Bool -> msg)
    -> String
    -> ( TranslationId, TranslationId )
    -> String
    -> Html msg
viewCustomBoolInput language currentValue setMsg inputClass ( yesTransId, noTransId ) inputWidth =
    let
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
        [ div [ class "ui grid" ]
            [ div [ class <| inputWidth ++ " wide column" ] <|
                viewInput True
            , div [ class <| inputWidth ++ " wide column" ] <|
                viewInput False
            ]
        ]


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
            label []
                [ translateFunc option |> translate language |> text ]
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


viewStartEncounterButton : Language -> msg -> Html msg
viewStartEncounterButton language action =
    viewEncounterActionButton language Translate.StartEncounter "primary" True action


viewEndEncounterButton : Language -> Bool -> (Bool -> msg) -> Html msg
viewEndEncounterButton language =
    viewEndEncounterButtonCustomColor language "primary"


viewEndEncounterButtonCustomColor : Language -> String -> Bool -> (Bool -> msg) -> Html msg
viewEndEncounterButtonCustomColor language buttonColor allowEndEncounter setDialogStateMsgs =
    viewEncounterActionButton language Translate.EndEncounter buttonColor allowEndEncounter (setDialogStateMsgs True)


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


viewEndEncounterMenuForProgressReport : Language -> Bool -> (Bool -> msg) -> msg -> Html msg
viewEndEncounterMenuForProgressReport language allowEndEncounter setDialogStateMsg setSendViaWhatsAppDialogStateMsg =
    let
        sendViaWhatsAppEnabled =
            -- For now, we disabled 'Send via WhatsApp' feature.
            False

        ( actionsClass, endEncounterButtonColor, sendViaWhatsAppButton ) =
            if sendViaWhatsAppEnabled then
                ( "actions two"
                , "velvet"
                , button
                    [ class "ui fluid primary button"
                    , onClick setSendViaWhatsAppDialogStateMsg
                    ]
                    [ text <| translate language Translate.SendViaWhatsApp ]
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
        , sendViaWhatsAppButton
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


tasksBarId : String
tasksBarId =
    "tasks-bar"


viewSaveAction : Language -> msg -> Bool -> Html msg
viewSaveAction language saveMsg disabled =
    div [ class "actions" ]
        [ saveButton language (not disabled) saveMsg ]


insertIntoSet : a -> Maybe (EverySet a) -> Maybe (EverySet a)
insertIntoSet value set =
    Maybe.map (EverySet.insert value) set
        |> Maybe.withDefault (EverySet.singleton value)
        |> Just


saveButton : Language -> Bool -> msg -> Html msg
saveButton language active msg =
    customSaveButton language active msg Translate.Save


customSaveButton : Language -> Bool -> msg -> Translate.TranslationId -> Html msg
customSaveButton language active msg label =
    button
        [ classList
            [ ( "ui fluid primary button", True )
            , ( "active", active )
            , ( "disabled", not active )
            ]
        , onClick msg
        ]
        [ text <| translate language label ]


customPopup : Language -> Bool -> TranslationId -> ( Html msg, Html msg, msg ) -> Html msg
customPopup language showWarning actionLabel ( topMessage, bottomMessage, action ) =
    div [ class "ui active modal diagnosis-popup" ]
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
