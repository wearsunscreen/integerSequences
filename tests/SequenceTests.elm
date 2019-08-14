module SequenceTests
    exposing
    -- (abundancyTests)
    ( abundancyTests
    , fibonacciTest
    , recamanTest
    , totientTest
    )

import Expect exposing (true)
import IntegerSequences exposing (..)
import Main exposing (..)
import Test exposing (..)


abundancyTests : Test
abundancyTests =
    describe "Test abundant, deficient and perfect numbers"
        [ test "Abundant number, negative input returns empty list" <|
            \() ->
                abundant -3
                    |> Expect.equal []
        , test "abundant 0 should be []" <|
            \() ->
                abundant 0
                    |> Expect.equal []
        , test "abundant 3 should be []" <|
            \() ->
                abundant 3
                    |> Expect.equal [ 12, 18, 20 ]
        , test "The 62th abundant number should be 270" <|
            \() ->
                abundant 62
                    |> List.drop 61
                    |> Expect.equal [ 270 ]
        ]


fibonacciTest : Test
fibonacciTest =
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


recamanTest : Test
recamanTest =
    describe "Test the Recamán sequence"
        [ test "Recaman length 0" <|
            \() ->
                recaman 0
                    |> Expect.equal []
        , test "Recaman length 1" <|
            \() ->
                recaman 1
                    |> Expect.equal [ 0 ]
        , test "Recaman length 2" <|
            \() ->
                recaman 2
                    |> Expect.equal [ 0, 1 ]
        , test "Recaman length 3" <|
            \() ->
                recaman 3
                    |> Expect.equal [ 0, 1, 3 ]
        , test "Recaman length 10" <|
            \() ->
                recaman 71
                    |> List.drop 61
                    |> Expect.equal [ 89, 27, 90, 26, 91, 157, 224, 156, 225, 155 ]
        , test "Recaman length 1002" <|
            \() ->
                recaman 100002
                    |> List.length
                    |> Expect.equal 10000
        ]


totientTest : Test
totientTest =
    describe "Totient tests"
        [ test "negative numbers, zero return zero" <|
            \() ->
                totient -3 0
                    |> Expect.equal [ 0, 0, 0, 0 ]
        , test "inverted arguments return an empty list" <|
            \() ->
                totient 10 5
                    |> Expect.equal []
        , test "first five" <|
            \() ->
                totient 1 5
                    |> Expect.equal [ 1, 1, 2, 2, 4 ]
        , test "totient 11 15" <|
            \() ->
                totient 11 15
                    |> Expect.equal [ 10, 4, 12, 6, 8 ]
        , test "totient 61 65" <|
            \() ->
                totient 61 65
                    |> Expect.equal [ 60, 30, 36, 32, 48 ]
        ]
