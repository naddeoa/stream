module Stream
    exposing
        ( Stream
        , filter
        , fromList
        , limit
        , map
        , next
        , nextN
        , reduce
        , toList
        , fibonocci
        , naturalNumbers
        , empty
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

This is obvious to anyone who feeld comfortable with functional programming, but if
you're coming from Java 8 then you're aware that you can't reuse streams
that have been collected.

# Types
@docs Stream

# Operations on streams
@docs limit, map, reduce, filter

# Getting things out of streams
@docs next, nextN, toList

# Creating streams
@docs fromList

# Special streams
@docs fibonocci, naturalNumbers, empty
-}

import Stream.Source as Source exposing (Source)


{-| Main type that represents a stream.
A Stream is a source of values that start as type `a` and end as type `b`. For most intents and purposes,
they can be thought of as collections of type `b`. Some streams are infinite and others are based on
pre-existing collections of data.

-}
type Stream a b
    = Stream (Source b)
    | MappedStream (a -> b) (Stream a a)
    | LimitedStream Int (Stream a b)
    | ListStream (List b)
    | FilteredStream (b -> Bool) (Stream a b)
    | ReducedStream (b -> b -> b) b (Stream a b)
    | Empty


{-| The empty stream. It has nothing in it. Calling next will
return nothing and calling toList will return an empty list. This
is useful for building upon.
-}
empty : Stream a b
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
map : (a -> b) -> Stream a a -> Stream a b
map f stream =
    MappedStream f stream


{-| Limit the size of a stream.
Make sure to call this at least once on a stream before calling `toList` on it; especially
for an infinite stream.

    -- [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    firstTenNumbers =
        Stream.naturalNumbers
            |> Stream.limit 10
            |> Stream.toList
-}
limit : Int -> Stream a b -> Stream a b
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
filter : (b -> Bool) -> Stream a b -> Stream a b
filter predicate stream =
    FilteredStream predicate stream


{-| Reduce a stream such that all values that come out of it are accumulations.
This is weird, but I like it. If you call `next` on a reduced stream then you'll
get the next accumulation. If you call `toList` then you'll get a list of accumulations.
In the following example, the first item is `1` because `0 + 1 = 1`. The second item
is `3` because the last accumulation was `1` and the next value in the natural number
seqauence is `2`, so `2 + 1 = 3`, etc.

    -- [ 1, 3, 6, 10, 15, 21, 28, 36, 45, 55 ]
    reduced =
        Stream.naturalNumbers
            |> Stream.limit 10
            |> Stream.reduce (+) 0
            |> Stream.toList

-}
reduce : (b -> b -> b) -> b -> Stream a b -> Stream a b
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
next : Stream a b -> ( Stream a b, Maybe b )
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


{-| An infinite stream of natural numbers starting from 1.

    -- [ 1, 2, 3 ]
    fist3Numbers =
        Stream.naturalNumbers
            |> Stream.limit 3
            |> Stream.toList
-}
naturalNumbers : Stream a Int
naturalNumbers =
    Stream Source.naturalNumbers


{-| Infinite stream of fibonocci numbers.

    -- [ 0, 1, 1, 2, 3, 5, 8, 13 ]
    first8Fib =
        Stream.naturalNumbers
            |> Stream.limit 8
            |> Stream.toList
-}
fibonocci : Stream ( Int, Int ) Int
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
fromList : List b -> Stream a b
fromList list =
    ListStream list



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
