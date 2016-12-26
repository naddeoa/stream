module Stream.Math exposing (..)

import Stream exposing (Stream)


factorsOf : Int -> Stream Int
factorsOf n =
    Stream.range (Basics.floor ((Basics.toFloat n) / 2)) 2 1
        |> Stream.filter (\m -> n % m == 0)


isPrime : Int -> Bool
isPrime n =
    factorsOf n
        |> Stream.isEmpty


primeFactorsOf : Int -> Stream Int
primeFactorsOf n =
    factorsOf n
        |> Stream.filter isPrime
