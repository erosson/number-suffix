module NumberSuffixDataTest exposing (suite)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import NumberSuffixData as S
import Test exposing (..)



-- https://package.elm-lang.org/packages/elm-explorations/test/latest


suite : Test
suite =
    describe "NumberSuffixData"
        [ test "standard" <|
            \_ ->
                S.standard |> Array.get 3 |> Expect.equal (Just " billion")
        , test "standardShort" <|
            \_ ->
                S.standardShort |> Array.get 3 |> Expect.equal (Just "B")
        , test "longScale" <|
            \_ ->
                S.longScale |> Array.get 3 |> Expect.equal (Just " milliard")
        , test "longScaleShort" <|
            \_ ->
                S.longScaleShort |> Array.get 3 |> Expect.equal (Just "Md")
        ]
