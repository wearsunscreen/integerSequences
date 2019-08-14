module IntegerSequences exposing (abundant, fibonacci, recaman, totient)

{-| This Elm package provides functions to generate integer sequences.
More information on each of the sequences can be found on [The On-Line
Encyclopedia of Integer Sequences® (OEIS®)](https://oeis.org).


# Integer sequences

@docs abundant, fibonacci, recaman, totient, random

-}


{-| Generates the sequence of abundant numbers. n is an abundant number if the
the sum of its divisors is greater than 2n.

This implementation will not return a series longer than 1000 integers.

[A005101 - OEIS](https://oeis.org/A005101)

    -- Generate a list of the first 3 abundant numbers
    abundant 3
    -- will evaluate to [12, 18, 20]

-}
abundant : Int -> List Int
abundant n =
    -- optimize knowing that 12 is the first abundant number
    let
        isAbundant : Int -> Bool
        isAbundant x =
            List.sum (divisors x) > (x * 2)

        addNext : (Int -> Bool) -> Int -> Int -> List Int -> List Int
        addNext pred end idx acc =
            let
                newAcc =
                    if pred idx then
                        acc ++ [ idx ]

                    else
                        acc
            in
            if List.length newAcc >= end then
                newAcc

            else
                addNext pred end (idx + 1) newAcc
    in
    addNext isAbundant n 1 []


{-| Generates the Fibonacci series. Each number is the sum for the
previous two number in the series. By defintion the first two elements
of the Fibonacci series are 0 and 1.

This implementation will not return a series
longer than 41 integers to avoid overly long processing.

[A000045 - OEIS](https://oeis.org/A000045)

    -- Generate a list of the first 6 Fibonacci numbers
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
                    -- subtract on to offset to zero index
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

This implementation will not return a series longer than 10000 integers.

[A005132 - OEIS](https://oeis.org/A005132)

[Numberphile Youtube video](https://www.youtube.com/watch?v=FGC5TdIiT9U)

    -- Generate a list of the first 3 Recamán numbers
    recaman 3
    -- will evaluate to [0, 1, 3]

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


{-| Calculates the Euler's Totient, the number of coprimes of a number.

[A000010 - OEIS](https://oeis.org/A000010)

    -- Calculate Euler's totient for 10, 11, 12, 13
    totient 10 13
    -- will evaluate to [ 4 10, 4, 12]

-}
totient : Int -> Int -> List Int
totient start end =
    List.range start end
        |> List.map totientOf



{- Utility functions -}


{-| Find all divisors of a number
-}
divisors : Int -> List Int
divisors n =
    List.range 1 (n // 2)
        |> List.filter (\d -> modBy d n == 0)
        |> (++) [ n ]


{-| find greastest common divisor
-}
gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd (abs b) (Basics.modBy b a)


isCoprime : Int -> Int -> Bool
isCoprime y z =
    gcd y z == 1


totientOf : Int -> Int
totientOf x =
    if x < 1 then
        0

    else
        List.range 0 (x - 1)
            |> List.map (isCoprime x)
            |> List.filter ((==) True)
            |> List.length
