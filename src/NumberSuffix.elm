module NumberSuffix exposing
    ( format, formatInt, formatSigExp
    , Config, standardConfig, scientificConfig, Locale
    , suffixStandard, suffixStandardShort, suffixEngineering, suffixLongScale, suffixLongScaleShort, suffixAlphabetic
    )

{-| Format numbers with fancy suffixes.


# Formatting

@docs format, formatInt, formatSigExp


# Configuration

@docs Config, standardConfig, scientificConfig, Locale


# Suffix list configuration

You'll usually use one of the built-in suffix lists below, but you could write your own. Here's a modified scientific notation suffix generator:

    suffixPow10 : Int -> String
    suffixPow10 digits = " * 10 ^ " ++ String.fromInt digits

    config : Config
    config = { scientificConfig | getSuffix = suffixPow10 }

    format config 1e6 --> "1.00 * 10 ^ 6"

@docs suffixStandard, suffixStandardShort, suffixEngineering, suffixLongScale, suffixLongScaleShort, suffixAlphabetic

-}

import Array exposing (Array)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import NumberSuffixData


{-| Configure how numbers are formatted.

`getSuffix` returns a suffix, given a digit count for the number. See the [suffix functions](#suffixStandard) below.

`sigfigs` is the number of significant figures shown.

Below `minSuffix`, a comma-separated number is shown instead of a suffixed number.

-}
type alias Config =
    { getSuffix : Int -> String
    , locale : Locale
    , sigfigs : Int
    , suffixDivisor : Int
    , minSuffix : Float
    }


{-| Format numbers differently based on the user's location and culture.

See `cuducos/elm-format-number:FormatNumber.Locales`.

This does not change the language suffixes are in, only the formatting of the
numbers themselves. Consider this with a custom suffix list if you need complete
internationalization.

    import FormatNumber.Locales

    spanishConfig : Config
    spanishConfig = { standardConfig | locale = FormatNumber.Locales.spanishLocale }

    format standardConfig 1234 --> "1,234"
    format spanishConfig 1234 --> "1.234"

-}
type alias Locale =
    FormatNumber.Locales.Locale


getListSuffix : Array String -> Int -> String
getListSuffix suffixes digits =
    case Array.get (digits // 3) suffixes of
        Just s ->
            s

        Nothing ->
            suffixEngineering digits


{-| Standard suffixes.

    config : Config
    config = { standardConfig | getSuffix = suffixStandard }
    -- `config = standardConfig` would work too; this is the default

    format config 1e3 --> "1,000"
    format config 1e5 --> "100 thousand"
    format config 1e6 --> "1.00 million"
    format config 1e9 --> "1.00 billion"
    format config 1e12 --> "1.00 trillion"
    format config 1e15 --> "1.00 quadrillion"

-}
suffixStandard : Int -> String
suffixStandard =
    getListSuffix NumberSuffixData.standard


{-| Abbreviated standard suffixes.

    config : Config
    config = { standardConfig | getSuffix = suffixStandardShort }

    format config 1e3 --> "1,000"
    format config 1e5 --> "100K"
    format config 1e6 --> "1.00M"
    format config 1e9 --> "1.00B"
    format config 1e12 --> "1.00T"
    format config 1e15 --> "1.00Qa"

-}
suffixStandardShort : Int -> String
suffixStandardShort =
    getListSuffix NumberSuffixData.standardShort


{-| Long-scale suffixes.

    config : Config
    config = { standardConfig | getSuffix = suffixLongScale }

    format config 1e3 --> "1,000"
    format config 1e5 --> "100 thousand"
    format config 1e6 --> "1.00 million"
    format config 1e9 --> "1.00 milliard"
    format config 1e12 --> "1.00 billion"
    format config 1e15 --> "1.00 billiard"

-}
suffixLongScale : Int -> String
suffixLongScale =
    getListSuffix NumberSuffixData.longScale


{-| Abbreviated long-scale suffixes.

    config : Config
    config = { standardConfig | getSuffix = suffixLongScaleShort }

    format config 1e3 --> "1,000"
    format config 1e5 --> "100K"
    format config 1e6 --> "1.00M"
    format config 1e9 --> "1.00Md"
    format config 1e12 --> "1.00B"
    format config 1e15 --> "1.00Bd"

-}
suffixLongScaleShort : Int -> String
suffixLongScaleShort =
    getListSuffix NumberSuffixData.longScaleShort


suffixScientific : Int -> String
suffixScientific digits =
    if digits <= 3 then
        ""

    else
        "e" ++ String.fromInt digits


{-| Engineering notation.

Unlike scientific notation, engineering notation numbers are always divisible by 3.

    config : Config
    config = { standardConfig | getSuffix = suffixEngineering }

    format config 1e3 --> "1,000"
    format config 1e5 --> "100E3"
    format config 1e6 --> "1.00E6"
    format config 1e7 --> "10.0E6"
    format config 1e8 --> "100E6"
    format config 1e9 --> "1.00E9"

-}
suffixEngineering : Int -> String
suffixEngineering digits =
    if digits <= 3 then
        ""

    else
        "E" ++ String.fromInt (digits // 3 * 3)


{-| Alphabetic suffixes.

    config : Config
    config = { standardConfig | getSuffix = suffixAlphabetic }

    format config 1e3 --> "1,000"
    format config 1e5 --> "100K"
    format config 1e6 --> "1.00M"
    format config 1e9 --> "1.00B"
    format config 1e12 --> "1.00T"
    format config 1e15 --> "1.00aa"
    format config 1e18 --> "1.00ab"

-}
suffixAlphabetic : Int -> String
suffixAlphabetic =
    getListSuffix NumberSuffixData.alphabetic


{-| Default formatting configuration.

By default, we use standard suffixes, US locale, 3 significant figures,
thousands grouping (suffixDivisor=3), and show no suffixes for values below 100,000.

-}
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


{-| Scientific notation formatting configuration.

    config : Config
    config = scientificConfig

    format config 1.0e3 --> "1,000"
    format config 1.0e6 --> "1.00e6"
    format config 1.0e7 --> "1.00e7"
    format config 1.0e8 --> "1.00e8"
    format config 1.0e9 --> "1.00e9"

-}
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

    config : Config
    config = { standardConfig | getSuffix = suffixStandardShort }
    format config 1.23e10 --> "12.3B"

-}
format : Config -> Float -> String
format config n =
    let
        exp =
            countDigits n - 1

        sig =
            n / toFloat (10 ^ exp)
    in
    formatSigExp config sig exp


formatSigExp : Config -> Float -> Int -> String
formatSigExp { getSuffix, sigfigs, locale, suffixDivisor, minSuffix } sig0 exp =
    let
        formatLocale =
            formatLocaleSigfigs locale sigfigs

        sig =
            sig0 * toFloat (10 ^ (exp |> abs |> Basics.remainderBy suffixDivisor))

        -- n could hit infinity for a large exponent; that's fine
        n =
            sig0 * toFloat (10 ^ exp)
    in
    if abs n < minSuffix then
        n |> formatLocale |> dropIntDecimals

    else
        formatLocale sig ++ getSuffix exp


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
