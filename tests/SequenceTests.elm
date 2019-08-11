module SequenceTests exposing
    ( fibonacciTest
    , recamanTest
    )

import Expect exposing (true)
import IntegerSequences exposing (..)
import Main exposing (..)
import Test exposing (..)


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
