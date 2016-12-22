module SourceTests exposing (..)

import Test exposing (..)
import Expect
import String
import Stream.Source as Source


all : Test
all =
    describe "Streams"
        [ test "first 8 fibonocci numbers" <|
            \() ->
                let
                    ( _, actual ) =
                        Source.fibonocci
                            |> Source.nextN 8

                    expected =
                        [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 5, 8 ), ( 8, 13 ), ( 13, 21 ) ]
                in
                    Expect.equalLists expected actual
        , test "first 100 natural numbers" <|
            \() ->
                let
                    ( _, actual ) =
                        Source.naturalNumbers
                            |> Source.nextN 100

                    expected =
                        (List.range 1 100)
                in
                    Expect.equalLists expected actual
        , test "value source" <|
            \() ->
                let
                    ( _, actual ) =
                        Source.value 'a'
                            |> Source.nextN 5

                    expected =
                        [ 'a', 'a', 'a', 'a', 'a' ]
                in
                    Expect.equalLists expected actual
        , test "current" <|
            \() ->
                let
                    expected =
                        1

                    actual =
                        Source.current Source.naturalNumbers
                in
                    Expect.equal expected actual
        , test "peek" <|
            \() ->
                let
                    expected =
                        2

                    actual =
                        Source.peek Source.naturalNumbers
                in
                    Expect.equal expected actual
        ]
