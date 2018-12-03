module NumberSuffix exposing
    ( format, formatInt
    , Config, standardConfig, scientificConfig, Locale
    , suffixStandard, suffixStandardShort, suffixEngineering, suffixLongScale, suffixLongScaleShort, suffixAlphabetic
    )

{-| Format numbers with fancy suffixes.


# Formatting

@docs format, formatInt


# Configuration

@docs Config, standardConfig, scientificConfig, Locale


# Suffix list configuration

@docs suffixStandard, suffixStandardShort, suffixEngineering, suffixLongScale, suffixLongScaleShort, suffixAlphabetic

-}

import Array exposing (Array)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import NumberSuffixData


type alias Locale =
    FormatNumber.Locales.Locale


type alias Config =
    { getSuffix : Int -> String
    , locale : Locale
    , sigfigs : Int
    , suffixDivisor : Int
    , minSuffix : Float
    }


getListSuffix : Array String -> Int -> String
getListSuffix suffixes digits =
    case Array.get (digits // 3) suffixes of
        Just s ->
            s

        Nothing ->
            suffixEngineering digits


suffixStandard : Int -> String
suffixStandard =
    getListSuffix NumberSuffixData.standard


suffixAlphabetic : Int -> String
suffixAlphabetic =
    getListSuffix NumberSuffixData.alphabetic


suffixStandardShort : Int -> String
suffixStandardShort =
    getListSuffix NumberSuffixData.standardShort


suffixLongScale : Int -> String
suffixLongScale =
    getListSuffix NumberSuffixData.longScale


suffixLongScaleShort : Int -> String
suffixLongScaleShort =
    getListSuffix NumberSuffixData.longScaleShort


suffixScientific : Int -> String
suffixScientific digits =
    if digits <= 3 then
        ""

    else
        "e" ++ String.fromInt digits


suffixEngineering : Int -> String
suffixEngineering digits =
    if digits <= 3 then
        ""

    else
        "E" ++ String.fromInt (digits // 3 * 3)


standardConfig : Config
standardConfig =
    { getSuffix = suffixStandard

    -- Surprisingly, the default is − (U+2212) instead of - (U+002D, I think).
    -- I suppose it's also-technically-correct, but it keeps tripping me up.
    , locale = { usLocale | negativePrefix = "-" }
    , sigfigs = 3
    , suffixDivisor = 3
    , minSuffix = 100000
    }


scientificConfig : Config
scientificConfig =
    { standardConfig
        | getSuffix = suffixScientific
        , suffixDivisor = 1
    }


countDigits : Float -> Int
countDigits =
    abs
        >> logBase 10
        -- elm's implementation of logBase has unacceptable floating-point imprecision:
        -- `logbase 10 1e6 == 5.99999999999`
        -- adding a tiny amount works around it. Awkward, hacky, but seems to work.
        -- wish I could just use the browser's Math.log10.
        >> roundTo round 9
        >> floor
        >> (+) 1
        -- special case: countDigits(0) should be 1, not -Infinity
        >> max 1


roundTo : (Float -> Int) -> Int -> Float -> Float
roundTo rounder roundExp val =
    let
        roundConst =
            10 ^ toFloat roundExp
    in
    (val * roundConst |> rounder |> toFloat) / roundConst


formatLocaleSigfigs : Locale -> Int -> Float -> String
formatLocaleSigfigs locale sigfigs val =
    let
        decimals =
            sigfigs - countDigits val |> max 0
    in
    val
        -- I want FormatNumber to floor its input, not round it. Sigh.
        -- er, truncate, not floor - round negatives toward zero.
        |> roundTo truncate decimals
        |> FormatNumber.format { locale | decimals = decimals }


{-| Format numbers with fancy suffixes.

    format standardConfig 12345 --> "12,345"

    format standardConfig 1.23e10 --> "12.3 billion"

    format { standardConfig | getSuffix = suffixStandardShort } 1.23e10 --> "12.3B"

-}
format : Config -> Float -> String
format { getSuffix, sigfigs, locale, suffixDivisor, minSuffix } n =
    let
        fdigits =
            countDigits n - 1

        formatLocale =
            formatLocaleSigfigs locale sigfigs

        significand =
            -- The below `x // y * y` looks silly at first, but it's integer division.
            -- This adjusts the significand to the nearest 1000 (for suffixDivisor=3).
            n / (10 ^ ((fdigits // suffixDivisor) * suffixDivisor |> toFloat))
    in
    if abs n < minSuffix then
        formatLocale n |> dropIntDecimals

    else
        formatLocale significand ++ getSuffix fdigits


{-| for example, `dropZeroDecimals "3.00" = "3"`
-}
dropIntDecimals : String -> String
dropIntDecimals val =
    case val |> String.split "." of
        head :: tail :: _ ->
            if tail == String.repeat (String.length tail) "0" then
                head

            else
                val

        _ ->
            val


{-| Format integers with fancy suffixes. See `format`.
-}
formatInt : Config -> Int -> String
formatInt config =
    toFloat >> format config
