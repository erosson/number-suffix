module NumberSuffixTest exposing (suite)

import Array
import Expect exposing (Expectation)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Fuzz exposing (Fuzzer, int, list, string)
import NumberSuffix as S exposing (standardConfig)
import Test exposing (..)



-- https://package.elm-lang.org/packages/elm-explorations/test/latest


formatFloat =
    FormatNumber.format usLocale


equalityTests : (a -> String) -> (a -> comparable) -> List ( a, comparable ) -> List Test
equalityTests toName fn =
    let
        each ( val, expected ) =
            test (toName val) <|
                \_ ->
                    val |> fn |> Expect.equal expected
    in
    List.map each


testFormat config val str =
    test ("format " ++ formatFloat val ++ " == \"" ++ str ++ "\"") <|
        \_ ->
            val |> S.format config |> Expect.equal str


suite : Test
suite =
    describe "suite"
        [ describe "standard" <|
            let
                run =
                    testFormat standardConfig
            in
            [ run 0 "0"
            , run 3 "3"
            , run 3.0e6 "3.00 million"
            , run 3.0e7 "30.0 million"
            , run 3.0e8 "300 million"
            , run 3.21e6 "3.21 million"
            , run 3.21e7 "32.1 million"
            , run 3.0e3 "3,000"
            , run 3.21e3 "3,210"
            , run 99999 "99,999"
            , run (99999 + 1) "100 thousand"
            , run -3 "-3"
            , run -3.0e6 "-3.00 million"
            , run -99999 "-99,999"
            , run -(99999 + 1) "-100 thousand"
            ]
        , describe "engineering" <|
            let
                run =
                    testFormat { standardConfig | getSuffix = S.suffixEngineering }
            in
            [ run 0 "0"
            , run 3 "3"
            , run 3.0e6 "3.00E6"
            , run 3.0e7 "30.0E6"
            , run 3.0e8 "300E6"
            , run 3.21e6 "3.21E6"
            , run 3.21e7 "32.1E6"
            , run 3.0e3 "3,000"
            , run 3.21e3 "3,210"
            , run 99999 "99,999"
            , run (99999 + 1) "100E3"
            , run -3 "-3"
            , run -3.0e6 "-3.00E6"
            , run -99999 "-99,999"
            , run -(99999 + 1) "-100E3"
            ]
        , describe "scientific" <|
            let
                run =
                    testFormat S.scientificConfig
            in
            [ run 0 "0"
            , run 1.0e0 "1"
            , run 1.0e1 "10"
            , run 1.0e2 "100"
            , run 1.0e3 "1,000"
            , run 1.0e4 "10,000"
            , run 1.0e5 "1.00e5"
            , run 1.0e6 "1.00e6"
            , run 1.0e7 "1.00e7"
            , run 1.0e8 "1.00e8"
            , run 1.0e9 "1.00e9"
            , run 9.99e9 "9.99e9"
            , run 2.0e9 "2.00e9"
            , run 1.0e10 "1.00e10"
            , only <| run 1.0e100 "1.00e100"
            , run -1.0e100 "-1.00e100"
            , run 1.234e7 "1.23e7"
            , run -1.234e7 "-1.23e7"
            ]
        , describe "custom suffix" <|
            let
                run =
                    testFormat { standardConfig | getSuffix = String.fromInt >> (++) "foobar" }
            in
            [ run 0 "0"
            , run 3.0e6 "3.00foobar6"
            ]
        , describe "sigfigs " <|
            let
                run =
                    testFormat { standardConfig | sigfigs = 4 }
            in
            [ run 0 "0"
            , run 3 "3"
            , run 3.0e6 "3.000 million"
            , run 3.0e7 "30.00 million"
            , run 3.0e8 "300.0 million"
            , run 3.21e6 "3.210 million"
            , run 3.21e7 "32.10 million"
            , run 3.0e3 "3,000"
            , run 3.21e3 "3,210"
            , run 99999 "99,999"
            , run (99999 + 1) "100.0 thousand"
            , run -3 "-3"
            , run -3.0e6 "-3.000 million"
            , run -99999 "-99,999"
            , run -(99999 + 1) "-100.0 thousand"
            ]
        , describe "min suffix" <|
            let
                run =
                    testFormat { standardConfig | minSuffix = 0 }
            in
            [ run 0 "0.00"
            , run 3 "3.00"
            , run 3.0e6 "3.00 million"
            , run 99999 "99.9 thousand"
            , run 9999 "9.99 thousand"
            , run 9994 "9.99 thousand"
            , run 9900 "9.90 thousand"
            , run 9995 "9.99 thousand"
            , run 1000 "1.00 thousand"
            ]
        ]
