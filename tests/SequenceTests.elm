module SequenceTests exposing (fibonacciTests)

import Expect exposing (true)
import IntegerSequences exposing (..)
import Main exposing (..)
import Test exposing (..)


fibonacciTests : Test
fibonacciTests =
    describe "Test Fibonacci series"
        [ test "Fibonacci series 1" <|
            \() ->
                fibonacci 1
                    |> Expect.equal [ 0 ]
        , test "Fibonacci series 2" <|
            \() ->
                fibonacci 2
                    |> Expect.equal [ 0, 1 ]
        , test "Fibonacci series 3" <|
            \() ->
                fibonacci 3
                    |> Expect.equal [ 0, 1, 1 ]
        , test "Fibonacci series" <|
            \() ->
                fibonacci 13
                    |> Expect.equal [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144 ]
        , test "Fibonacci long test" <|
            \() ->
                fibonacci 41
                    |> List.drop 40
                    |> Expect.equal [ 102334155 ]
        , test "Fibonacci super long test" <|
            \() ->
                fibonacci 50
                    |> List.length
                    |> Expect.lessThan 50
        , test "Fibonacci series - zero length" <|
            \() ->
                fibonacci 0
                    |> Expect.equal []
        , test "Fibonacci series - negative argument" <|
            \() ->
                fibonacci -1
                    |> Expect.equal []
        ]
