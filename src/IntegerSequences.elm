module IntegerSequences exposing
    ( fibonacci
    , recaman
    )

{-| This Elm package provides functions to generate integer sequences.
More information on each of the sequences can be found on [The On-Line
Encyclopedia of Integer Sequences® (OEIS®)](https://oeis.org).


# Integer sequences

@docs fibonacci, random

-}


{-| Generates the Fibonacci series. Each number is the sum for the
previous two number in the series. By defintion the first two elements
of the Fibonacci series are 0 and 1.

This implementation will not return series
longer than 41 integers to avoid overly long processing.

[A000045 - OEIS](https://oeis.org/A000045)

    -- Generate a list of the first 5 Fibonacci numbers
    fibonacci 6
    -- will evaluate to [0, 1, 2, 3, 5, 8]

-}
fibonacci : Int -> List Int
fibonacci n =
    case max n 0 of
        0 ->
            []

        _ ->
            let
                ne =
                    min n 41 - 1

                fib x =
                    case x of
                        0 ->
                            0

                        1 ->
                            1

                        _ ->
                            fib (x - 1) + fib (x - 2)
            in
            0 :: List.map fib (List.range 1 ne)


{-| Generates the Recamán series.

This implementation will not return series longer than 10000 integers.

[A005132 - OEIS](https://oeis.org/A005132)

[Numberphile Youtube video](https://www.youtube.com/watch?v=FGC5TdIiT9U)

    -- Generate a list of the first 5 Recamán numbers
    recaman 6
    -- will evaluate to [0, 1, 2, 3, 5, 8]

-}
recaman : Int -> List Int
recaman n =
    case max n 0 of
        0 ->
            []

        1 ->
            [ 0 ]

        _ ->
            let
                ne =
                    min n 10000

                rec : Int -> Int -> Int -> List Int -> List Int
                rec len atPrevIdx idx acc =
                    if idx >= len then
                        acc

                    else
                        let
                            x =
                                atPrevIdx - idx

                            atCurIdx =
                                if x < 0 || List.member x acc then
                                    atPrevIdx + idx

                                else
                                    atPrevIdx - idx
                        in
                        rec len atCurIdx (idx + 1) (acc ++ [ atCurIdx ])
            in
            rec ne 0 1 [ 0 ]
