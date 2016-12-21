module Stream exposing (..)

import Stream.Source as Source exposing (Source)


type Stream a b
    = Stream (Source b)
    | MappedStream (a -> b) (Stream a a)
    | LimitedStream Int (Stream a b)
    | ListStream (List b)
    | FilteredStream (b -> Bool) (Stream a b)
    | ReducedStream (b -> b -> b) b (Stream a b)



--  Functions over Streams


map : (a -> b) -> Stream a a -> Stream a b
map f stream =
    MappedStream f stream


limit : Int -> Stream a b -> Stream a b
limit n stream =
    LimitedStream n stream


filter : (b -> Bool) -> Stream a b -> Stream a b
filter predicate stream =
    FilteredStream predicate stream


reduce : (b -> b -> b) -> b -> Stream a b -> Stream a b
reduce reducer seed stream =
    ReducedStream reducer seed stream


next : Stream a b -> ( Stream a b, Maybe b )
next stream =
    case stream of
        Stream source ->
            let
                ( nextSource, nextValue ) =
                    Source.take source
            in
                ( Stream nextSource, Just <| Source.current source )

        MappedStream f baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( MappedStream f nextStream, Nothing )

                    Just b ->
                        ( MappedStream f nextStream, Just <| f b )

        LimitedStream n baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( LimitedStream 0 baseStream, Nothing )

                    Just b ->
                        case n <= 0 of
                            True ->
                                ( LimitedStream 0 baseStream, Nothing )

                            False ->
                                ( LimitedStream (n - 1) nextStream, Just b )

        ListStream list ->
            case List.head list of
                Nothing ->
                    ( ListStream list, Nothing )

                Just b ->
                    ( ListStream <| Maybe.withDefault [] (List.tail list), Just b )

        FilteredStream predicate baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( FilteredStream predicate baseStream, Nothing )

                    Just b ->
                        let
                            nextFilter =
                                FilteredStream predicate nextStream
                        in
                            if predicate b then
                                ( nextFilter, Just b )
                            else
                                next nextFilter

        ReducedStream reducer seed baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( ReducedStream reducer seed baseStream, Nothing )

                    Just b ->
                        let
                            reducedValue =
                                reducer seed b
                        in
                            ( ReducedStream reducer reducedValue nextStream, Just reducedValue )


nextN : Int -> Stream a b -> ( Stream a b, List b )
nextN n stream =
    nextNHelper n stream []


nextNHelper : Int -> Stream a b -> List b -> ( Stream a b, List b )
nextNHelper n stream acc =
    case n <= 0 of
        True ->
            ( stream, List.reverse acc )

        False ->
            let
                ( nextStream, nextValue ) =
                    next stream
            in
                case nextValue of
                    Nothing ->
                        ( stream, List.reverse acc )

                    Just a ->
                        nextNHelper (n - 1) nextStream (a :: acc)



-- Special streams


naturalNumbers : Stream a Int
naturalNumbers =
    Stream Source.naturalNumbers


fibonocci : Stream ( Int, Int ) Int
fibonocci =
    map Tuple.first <| Stream (Source.fibonocci)



-- Creating streams


fromList : List b -> Stream a b
fromList list =
    ListStream list



-- Gathering streams


toList : Stream a b -> List b
toList stream =
    toListHelper stream []
        |> Tuple.second


toListHelper : Stream a b -> List b -> ( Stream a b, List b )
toListHelper stream acc =
    let
        ( nextStream, nextValue ) =
            next stream
    in
        case nextValue of
            Nothing ->
                ( stream, List.reverse acc )

            Just a ->
                toListHelper nextStream (a :: acc)
