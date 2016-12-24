module Stream
    exposing
        ( Stream
        , filter
        , takeWhile
        , dropWhile
        , fromList
        , limit
        , map
        , map2
        , zip
        , next
        , nextN
        , reduce
        , toList
        , fibonocci
        , naturalNumbers
        , empty
        , value
        , range
        , singleton
        , isEmpty
        , iterate
        , cycle
        )

{-| A `Stream` is kind of like a stream in Java 8 and kind of like a lazy list. It is a
potentially infinite stream of items that can be transformed and collected back into
other structures.

This library was made because Elm currently doesn't have great options for
lazyiness. There are a few implementations that exist but they are vulnerable
to stack overflows. This library uses tail call recursion for collecting
streams into lists. Also, instead of modeling the streams recursively it uses
an approach closer to the Elm architecture. The `Lazy` core library is not
used. The deepest stack you will have is equal to the amount of transformations
you apply on a stream.  For example, in the following snippet:

    let
        finalStream =
            Stream.fibonocci
                |> Stream.limit 100
                |> Stream.filter isEven
                |> Stream.map toString

Every time `next` is called on `finalStream` a stack of size three is generated
before a value can be returned. It scales with the number of transforms, not
the size of the list. You would have to apply a high enough number of
transforms to a stream in order to get a stack overflow. That number is
high enough that you won't have to worry about it. That said, in other
libraries when you attempt to turn an infinite stream into a list you get a
stack overflow. In this library, it will just run forever, so, keep that in
mind.

One useful difference from Java 8 streams that we get just for being functional is
stream reuse. Things like the following are possible now.

    let
        baseStream =
            Stream.naturalNumbers
                |> Stream.limit 10

        firstList =
            Stream.toList baseStream

        list =
            baseStream
                |> Stream.map toString
                |> Stream.toList

This is obvious to anyone who feels comfortable with functional programming, but if
you're coming from Java 8 then you're aware that you can't reuse streams
that have been collected.

# Types
@docs Stream

# Operations on streams
@docs limit, reduce, filter, takeWhile, dropWhile, isEmpty, map, map2, fibonocci, zip

# Getting things out of streams
@docs next, nextN, toList

# Creating streams
@docs fromList, value, singleton, range, iterate, cycle

# Special streams
@docs naturalNumbers, empty
-}

import Stream.Source as Source exposing (Source)
import Lazy exposing (Lazy, lazy, force)


{-| Main type that represents a stream.
A Stream is a source of values that start as type `a` and end as type `b`. For most intents and purposes,
they can be thought of as collections of type `b`. Some streams are infinite and others are based on
pre-existing collections of data.

-}
type Stream b
    = Stream (Source b)
    | MappedStream b (Lazy (Stream b))
    | LimitedStream Int (Stream b)
    | ListStream (List b)
    | FilteredStream (b -> Bool) (Stream b)
    | TakeWhileStream (b -> Bool) (Stream b)
    | DropWhileStream (b -> Bool) (Stream b)
    | ReducedStream (b -> b -> b) b (Stream b)
    | CycleStream (Stream b) (Stream b)
    | Empty


{-| The empty stream. It has nothing in it. Calling next will
return nothing and calling toList will return an empty list. This
is useful for building upon.
-}
empty : Stream b
empty =
    Empty



--  Functions over Streams


{-| Map a stream from one that returns a type a to one that returns a type b

    -- [ 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 ]
    firstTenNumbresTimesTwo =
        Stream.naturalNumbers
            |> Stream.limit 10
            |> Stream.map (* 2)
            |> Stream.toList
-}
map : (a -> b) -> Stream a -> Stream b
map f stream =
    let
        ( nextStream, nextValue ) =
            next stream
    in
        case nextValue of
            Nothing ->
                Empty

            Just a ->
                MappedStream (f a) (lazy (\() -> map f nextStream))


{-| Like `map`, but for two streams instead of one.

    -- [ "1a", "2a", "3a", "4a", "5a" ]
    actual =
        Stream.value "a"
            |> Stream.map2 (\a b -> toString a ++ b) (Stream.range 1 5 1)
            |> Stream.toList
-}
map2 : (a -> b -> c) -> Stream a -> Stream b -> Stream c
map2 f stream1 stream2 =
    let
        ( nextStream1, nextValue1 ) =
            next stream1

        ( nextStream2, nextValue2 ) =
            next stream2
    in
        case nextValue1 of
            Nothing ->
                Empty

            Just a ->
                case nextValue2 of
                    Nothing ->
                        Empty

                    Just b ->
                        MappedStream (f a b) (lazy (\() -> map2 f nextStream1 nextStream2))


{-| Zip two streams together.

    -- [ ( 1, "a" ), ( 2, "a" ), ( 3, "a" ), ( 4, "a" ), ( 5, "a" ) ]
    zippedStream =
        Stream.value "a"
            |> Stream.zip (Stream.range 1 5 1)
            |> Stream.toList
-}
zip : Stream a -> Stream b -> Stream ( a, b )
zip stream1 stream2 =
    map2 (,) stream1 stream2


{-| Limit the size of a stream.
Make sure to call this at least once on a stream before calling `toList` on it; especially
for an infinite stream.

    -- [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    firstTenNumbers =
        Stream.naturalNumbers
            |> Stream.limit 10
            |> Stream.toList
-}
limit : Int -> Stream b -> Stream b
limit n stream =
    LimitedStream n stream


{-| Filter values from a stream.
Keep only the values in a stream that match a predicate. Make sure to call
`limit` on an infinite stream before using this one or you'll end up
running forever when you finally try to turn it into a list.

    -- [ 2, 4, 6, 8 ]
    evenNumbers =
        Stream.naturalNumbers
            |> Stream.limit 10
            |> Stream.filter (\n -> n % 2 == 0)
            |> Stream.toList
-}
filter : (b -> Bool) -> Stream b -> Stream b
filter predicate stream =
    FilteredStream predicate stream


{-| Take items from a stream while a predicate is true.

    numbersUnder10 =
        Stream.naturalNumbers
            |> Stream.takeWhile (\n -> n < 10)
            |> Stream.toList
-}
takeWhile : (b -> Bool) -> Stream b -> Stream b
takeWhile predicate stream =
    TakeWhileStream predicate stream


{-| Drop items from a stream until a predicate is satisfied.

    -- [ 100, 101, 102, 103, 104, 105, 106, 107, 108, 109 ]
    from100To109=
        Stream.naturalNumbers
            |> Stream.dropWhile (\n -> n < 100)
            |> Stream.limit 10
            |> Stream.toList
-}
dropWhile : (b -> Bool) -> Stream b -> Stream b
dropWhile predicate stream =
    DropWhileStream predicate stream


{-| Reduce a stream such that all values that come out of it are accumulations.
This stream will always have a size of one. It is the result of a reduction on its base stream.

    -- [ 55 ]
    reduced =
        Stream.naturalNumbers
            |> Stream.limit 10
            |> Stream.reduce (+) 0
            |> Stream.toList

-}
reduce : (b -> b -> b) -> b -> Stream b -> Stream b
reduce reducer seed stream =
    ReducedStream reducer seed stream


{-| Get the next value out of a stream.
Returns the next `Maybe` from a stream, as well as the updated Stream. If this value
is `Nothing` then the stream is empty.

    -- Just 1
    nextNumber =
        Stream.fromList [ 1, 2, 3 ]
            |> Stream.next
            |> Tuple.second
-}
next : Stream b -> ( Stream b, Maybe b )
next stream =
    case stream of
        Empty ->
            ( stream, Nothing )

        Stream source ->
            let
                ( nextSource, nextValue ) =
                    Source.next source
            in
                ( Stream nextSource, Just <| Source.current source )

        MappedStream value lazy ->
            ( force lazy, Just value )

        LimitedStream n baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( Empty, Nothing )

                    Just b ->
                        case n <= 0 of
                            True ->
                                ( Empty, Nothing )

                            False ->
                                ( LimitedStream (n - 1) nextStream, Just b )

        ListStream list ->
            case List.head list of
                Nothing ->
                    ( Empty, Nothing )

                Just b ->
                    ( ListStream <| Maybe.withDefault [] (List.tail list), Just b )

        FilteredStream predicate baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( Empty, Nothing )

                    Just b ->
                        let
                            nextFilter =
                                FilteredStream predicate nextStream
                        in
                            if predicate b then
                                ( nextFilter, Just b )
                            else
                                next nextFilter

        TakeWhileStream predicate baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( Empty, Nothing )

                    Just b ->
                        if predicate b then
                            ( TakeWhileStream predicate nextStream, Just b )
                        else
                            ( Empty, Nothing )

        DropWhileStream predicate baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( Empty, Nothing )

                    Just b ->
                        if predicate b then
                            next <| DropWhileStream predicate nextStream
                        else
                            ( DropWhileStream predicate nextStream, Just b )

        ReducedStream reducer seed baseStream ->
            let
                ( nextStream, nextValue ) =
                    next baseStream
            in
                case nextValue of
                    Nothing ->
                        ( Empty, Nothing )

                    Just b ->
                        let
                            reducedValue =
                                reducer seed b
                        in
                            ( ReducedStream reducer reducedValue nextStream, Just reducedValue )

        CycleStream originalStream currentStream ->
            let
                ( nextStream, nextValue ) =
                    next currentStream
            in
                case nextValue of
                    Nothing ->
                        next (CycleStream originalStream originalStream)

                    Just b ->
                        ( CycleStream originalStream nextStream, Just b )


{-| Like `next`, but it retuns a `List` of values instead of a `Maybe` of a single value.
If you ask for more values than are in the list then you get whatever is in the list. In general,
functions like this are more of upper limits on streams. You won't get more than you ask for, but
you might get less.

    -- [ 1, 2, 3 ]
    entireList =
        Stream.fromList [ 1, 2, 3 ]
            |> Stream.nextN 10
            |> Tuple.second
-}
nextN : Int -> Stream b -> ( Stream b, List b )
nextN n stream =
    nextNHelper n stream []


nextNHelper : Int -> Stream b -> List b -> ( Stream b, List b )
nextNHelper n stream acc =
    case n <= 0 of
        True ->
            case stream of
                ReducedStream _ reduceSeed _ ->
                    ( stream, [ Maybe.withDefault reduceSeed (List.head acc) ] )

                _ ->
                    ( stream, List.reverse acc )

        False ->
            let
                ( nextStream, nextValue ) =
                    next stream
            in
                case nextValue of
                    Nothing ->
                        case stream of
                            ReducedStream _ reduceSeed _ ->
                                ( stream, [ Maybe.withDefault reduceSeed (List.head acc) ] )

                            _ ->
                                ( stream, List.reverse acc )

                    Just a ->
                        nextNHelper (n - 1) nextStream (a :: acc)



-- Special streams


{-| An infinite stream of natural numbers starting from 1.

    -- [ 1, 2, 3 ]
    fist3Numbers =
        Stream.naturalNumbers
            |> Stream.limit 3
            |> Stream.toList
-}
naturalNumbers : Stream Int
naturalNumbers =
    Stream Source.naturalNumbers


{-| Infinite stream of fibonocci numbers.

    -- [ 0, 1, 1, 2, 3, 5, 8, 13 ]
    first8Fib =
        Stream.naturalNumbers
            |> Stream.limit 8
            |> Stream.toList
-}
fibonocci : Stream Int
fibonocci =
    map Tuple.first <| Stream (Source.fibonocci)



-- Creating streams


{-| Create a stream from a list.
Streams created this way are not infinite (obviously). You don't need to
call `limit` on them or anything like you would on an infinite stream.

    -- [ 1, 2, 3 ]
    list =
        Stream.fromList [ 1, 2, 3 ]
            |> Stream.toList
-}
fromList : List b -> Stream b
fromList list =
    ListStream list


{-| Create an infinite stream that returns a single value forever.

    -- [ 'a', 'a', ... ]
    bunchOfA =
        Stream.value 'a'
-}
value : b -> Stream b
value b =
    Stream (Source.value b)


{-| Create a stream from `start` to `stop` with a step of `step`. The
stream will always include the start. If the stop value is less than the
start then the start value is used in its place.

    -- [ 1, 3, 5, 7, 9, 11 ]
    actual =
        Stream.range 1 11 2
            |> Stream.toList
-}
range : Int -> Int -> Int -> Stream Int
range start stop step =
    Stream (Source.iterate start ((+) step))
        |> takeWhile (\n -> n <= (Basics.max start stop))


{-| Create an infinite stream that iterates a function over a seed.

    -- [ "a", "aa", "aaa", "aaaa", "aaaaa" ]
    someAs =
        Stream.iterate "a" ((++) "a")
            |> Stream.limit 5
            |> Stream.toList
-}
iterate : b -> (b -> b) -> Stream b
iterate seed fn =
    Stream (Source.iterate seed fn)


{-| Create an infinite stream that cycles through values of another stream.

    -- [ "", "", "Fizz", "", "", "Fizz" ]
    fizzes =
        Stream.fromList [ "", "", "Fizz" ]
            |> Stream.cycle
            |> Stream.limit 6
            |> Stream.toList
-}
cycle : Stream b -> Stream b
cycle stream =
    if isEmpty stream then
        empty
    else
        CycleStream stream stream


{-| Create a stream of size 1 that contains a single value.

    -- [ 'a' ]
    justA =
        Stream.singleton 'a'
            |> Stream.toList
-}
singleton : b -> Stream b
singleton b =
    fromList [ b ]



-- Gathering streams


{-| Empty a Stream into a `List`.
Make sure you only call this on streams with limits on them or it will
run forever. Like, it will actually run forever, it won't overflow the stack or
anything, it will just run forever.

    -- [ 1, 2, 3 ]
    list =
        Stream.fromList [ 1, 2, 3 ]
            |> Stream.toList
-}
toList : Stream b -> List b
toList stream =
    toListHelper stream []
        |> Tuple.second


toListHelper : Stream b -> List b -> ( Stream b, List b )
toListHelper stream acc =
    let
        ( nextStream, nextValue ) =
            next stream
    in
        case nextValue of
            Nothing ->
                case stream of
                    ReducedStream _ reduceSeed _ ->
                        ( stream, [ Maybe.withDefault reduceSeed (List.head acc) ] )

                    _ ->
                        ( stream, List.reverse acc )

            Just a ->
                toListHelper nextStream (a :: acc)


{-| Whether or not this stream is empty.
The check is determined by whether or not the next value out of the stream
is `Nothing`
-}
isEmpty : Stream b -> Bool
isEmpty stream =
    case Tuple.second <| next stream of
        Nothing ->
            True

        _ ->
            False
