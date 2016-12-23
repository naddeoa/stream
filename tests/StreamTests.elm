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
        , test "value" <|
            \() ->
                let
                    expected =
                        [ 'a', 'a', 'a' ]

                    actual =
                        Stream.value 'a'
                            |> Stream.limit 3
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "singleton" <|
            \() ->
                let
                    expected =
                        [ 'a' ]

                    actual =
                        Stream.singleton 'a'
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "isEmpty True" <|
            \() ->
                let
                    expected =
                        True

                    actual =
                        Stream.isEmpty Stream.empty
                in
                    Expect.equal expected actual
        , test "isEmpty False" <|
            \() ->
                let
                    expected =
                        False

                    actual =
                        Stream.isEmpty Stream.naturalNumbers
                in
                    Expect.equal expected actual
        , test "basic range" <|
            \() ->
                let
                    expected =
                        List.range 1 10

                    actual =
                        Stream.range 1 10 1
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "range with a 2 step" <|
            \() ->
                let
                    expected =
                        [ 1, 3, 5, 7, 9, 11 ]

                    actual =
                        Stream.range 1 11 2
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "range with a 100 step past the stopping point" <|
            \() ->
                let
                    expected =
                        [ 1 ]

                    actual =
                        Stream.range 1 50 100
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "start and stop are equal" <|
            \() ->
                let
                    expected =
                        [ 1 ]

                    actual =
                        Stream.range 1 1 1
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "stop is less than start" <|
            \() ->
                let
                    expected =
                        [ 10 ]

                    actual =
                        Stream.range 10 1  1
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        ]
