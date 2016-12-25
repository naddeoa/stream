module EulerTests exposing (..)

import Test exposing (..)
import Expect
import String
import Stream


all : Test
all =
    describe "Project Eueler based test cases"
        [ describe "1: Find the sum of all the multiples of 3 or 5 below 1000"
            [ test "answer" <|
                \() ->
                    let
                        multiplesOf3 =
                            Stream.cycle <| Stream.fromList [ 0, 0, 1 ]

                        multiplesOf5 =
                            Stream.cycle <| Stream.fromList [ 0, 0, 0, 0, 1 ]

                        allMultiples =
                            multiplesOf3
                                |> Stream.map2 (+) multiplesOf5
                                |> Stream.zip (Stream.range 1 999 1)
                                |> Stream.filter (\( id, result ) -> result > 0)
                                |> Stream.map (\( id, result ) -> id)
                                |> Stream.reduce (+) 0
                                |> Stream.toList
                                |> List.head
                                |> Maybe.withDefault 0

                        answer =
                            233168
                    in
                        Expect.equal answer allMultiples
            ]
        , describe "2: By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms"
            [ test "answer" <|
                \() ->
                    let
                        sumOfEvens =
                            Stream.fibonocci
                                |> Stream.takeWhile (\n -> n < 4000000)
                                |> Stream.filter (\n -> n % 2 == 0)
                                |> Stream.reduce (+) 0
                                |> Stream.toList
                                |> List.head
                                |> Maybe.withDefault 0

                        answer =
                            4613732
                    in
                        Expect.equal answer sumOfEvens
            ]
        ]
