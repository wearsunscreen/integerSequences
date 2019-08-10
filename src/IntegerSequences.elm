module IntegerSequences exposing (fibonacci)

{-| This Elm package provides functions to generate integer sequences.
More information on each of the sequences can be found on [The On-Line
Encyclopedia of Integer Sequences® (OEIS®)](https://oeis.org).


# Integer sequences

@docs fibonacci, random

-}


{-| Generates the Fibonacci series. Each number is the sum for the
previous two number in the series. By defintion the first two elements
of the Fibonacci series are 0 and 1. [A000045 - OEIS](https://oeis.org/A000045)

    -- Generate a list of the first 5 Fibonacci numbers
    fibonacci 6
    -- will evaluate to [0, 1, 2, 3, 5, 8]

-}
fibonacci : Int -> List Int
fibonacci n =
    let
        fib x =
            case x of
                0 ->
                    0

                1 ->
                    1

                _ ->
                    fib (x - 1) + fib (x - 2)
    in
    List.take n
        (0 :: List.map fib (List.range 1 n))
