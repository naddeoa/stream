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
        , test "map2" <|
            \() ->
                let
                    actual =
                        Stream.value "a"
                            |> Stream.map2 (\a b -> toString a ++ b) (Stream.range 1 5 1)
                            |> Stream.toList

                    expected =
                        [ "1a", "2a", "3a", "4a", "5a" ]
                in
                    Expect.equalLists expected actual
        , test "zip" <|
            \() ->
                let
                    actual =
                        Stream.value "a"
                            |> Stream.zip (Stream.range 1 5 1)
                            |> Stream.toList

                    expected =
                        [ ( 1, "a" ), ( 2, "a" ), ( 3, "a" ), ( 4, "a" ), ( 5, "a" ) ]
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
        , test "filter" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 3, 4 ]

                    actual =
                        Stream.range 1 10 1
                            |> Stream.filter (\n -> n < 5)
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "drop" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 3, 4 ]

                    actual =
                        Stream.range 1 10 1
                            |> Stream.drop (\n -> n >= 5)
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "descending range" <|
            \() ->
                let
                    expected =
                        [ 5, 4, 3, 2, 1, 0 ]

                    actual =
                        Stream.range 5 0 1
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
        , test "iterate" <|
            \() ->
                let
                    expected =
                        [ "a", "aa", "aaa", "aaaa", "aaaaa" ]

                    actual =
                        Stream.iterate "a" ((++) "a")
                            |> Stream.limit 5
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "concat" <|
            \() ->
                let
                    expected =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

                    actual =
                        Stream.concat (Stream.range 0 4 1) (Stream.range 5 10 1)
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "concat one empty stream" <|
            \() ->
                let
                    expected =
                        [ 5, 6, 7, 8, 9, 10 ]

                    actual =
                        Stream.concat Stream.empty (Stream.range 5 10 1)
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "concat two empty streams" <|
            \() ->
                let
                    expected =
                        []

                    actual =
                        Stream.concat Stream.empty Stream.empty
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "flatten works" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 3, 4, 5 ]

                    nestedStream =
                        Stream.fromList
                            [ Stream.fromList [ 1, 2, 3 ]
                            , Stream.fromList []
                            , Stream.fromList [ 4, 5 ]
                            , Stream.fromList []
                            , Stream.fromList []
                            ]

                    actual =
                        Stream.flatten nestedStream
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "interleave works" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]

                    nestedStream =
                        [ Stream.fromList [ 1, 1, 1 ]
                        , Stream.fromList [ 2, 2, 2 ]
                        , Stream.fromList [ 3, 3, 3 ]
                        ]

                    actual =
                        Stream.interleave nestedStream
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "interleave works with empty lists" <|
            \() ->
                let
                    expected =
                        []

                    nestedStream =
                        [ Stream.fromList []
                        , Stream.fromList []
                        , Stream.fromList []
                        , Stream.fromList []
                        , Stream.fromList []
                        , Stream.fromList []
                        ]

                    actual =
                        Stream.interleave nestedStream
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "interleave exhausts all streams" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 3, 1, 2, 3, 2, 3, 3 ]

                    nestedStream =
                        [ Stream.fromList [ 1, 1 ]
                        , Stream.fromList [ 2, 2, 2 ]
                        , Stream.fromList [ 3, 3, 3, 3 ]
                        ]

                    actual =
                        Stream.interleave nestedStream
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "interleave with infinite streams" <|
            \() ->
                let
                    expected =
                        [ 1, 2, 1, 2, 1, 2, 1, 2, 1, 2 ]

                    nestedStream =
                        [ Stream.value 1
                        , Stream.value 2
                        ]

                    actual =
                        Stream.interleave nestedStream
                            |> Stream.limit 10
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "cycle fizz" <|
            \() ->
                let
                    expected =
                        [ "", "", "Fizz", "", "", "Fizz" ]

                    actual =
                        Stream.fromList [ "", "", "Fizz" ]
                            |> Stream.cycle
                            |> Stream.limit 6
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        , test "cycle of an empty stream is just an empty list" <|
            \() ->
                let
                    expected =
                        []

                    actual =
                        Stream.empty
                            |> Stream.cycle
                            |> Stream.limit 6
                            |> Stream.toList
                in
                    Expect.equalLists expected actual
        ]
