module StreamTests exposing (..)

import Test exposing (..)
import Expect
import String
import Stream


all : Test
all =
    describe "Streams"
        [ test "first 8 fibonocci numbers" <|
            \() ->
                let
                    actual =
                        Stream.fibonocci
                            |> Stream.limit 8
                            |> Stream.toList

                    expected =
                        [ 0, 1, 1, 2, 3, 5, 8, 13 ]
                in
                    Expect.equalLists expected actual
        , test "first 100 natural numbers" <|
            \() ->
                let
                    actual =
                        Stream.naturalNumbers
                            |> Stream.limit 100
                            |> Stream.toList

                    expected =
                        (List.range 1 100)
                in
                    Expect.equalLists expected actual
        , test "double numbers with map" <|
            \() ->
                let
                    actual =
                        Stream.naturalNumbers
                            |> Stream.limit 100
                            |> Stream.map (\n -> n * 2)
                            |> Stream.toList

                    expected =
                        List.map (\n -> n * 2) <| List.range 1 100
                in
                    Expect.equalLists expected actual
        , test "toList" <|
            \() ->
                let
                    expected =
                        List.range 0 1000

                    actual =
                        Stream.fromList expected
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "nextN" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 3, 4, 5 ]

                    ( _, actual ) =
                        Stream.naturalNumbers
                            |> Stream.nextN 5
                in
                    Expect.equalLists expected actual
        , test "nextN on a reduced stream" <|
            \() ->
                let
                    expected =
                        [ 15 ]

                    ( _, actual ) =
                        Stream.naturalNumbers
                            |> Stream.reduce (+) 0
                            |> Stream.nextN 5
                in
                    Expect.equalLists expected actual
        , test "reduce" <|
            \() ->
                let
                    expected =
                        [ 55 ]

                    actual =
                        Stream.naturalNumbers
                            |> Stream.limit 10
                            |> Stream.reduce (+) 0
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "reduce an empty stream" <|
            \() ->
                let
                    expected =
                        [ 0 ]

                    actual =
                        Stream.empty
                            |> Stream.reduce (+) 0
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "takeWhile" <|
            \() ->
                let
                    expected =
                        List.range 1 99

                    actual =
                        Stream.naturalNumbers
                            |> Stream.takeWhile (\n -> n < 100)
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "dropWhile" <|
            \() ->
                let
                    expected =
                        List.range 100 109

                    actual =
                        Stream.naturalNumbers
                            |> Stream.dropWhile (\n -> n < 100)
                            |> Stream.limit 10
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        ]
