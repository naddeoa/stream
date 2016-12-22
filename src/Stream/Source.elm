module Stream.Source
    exposing
        ( Source
        , next
        , nextN
        , peek
        , current
        , value
        , naturalNumbers
        , fibonocci
        )

{-|
This module exists mostly to support infinite streams in the `Stream` module.

@docs Source

# Getting things out
@docs next, nextN, peek, current

# Special sources
@docs value, naturalNumbers, fibonocci
-}


{-| The main Source type.
As a rule, all `Source`s are infinite. They are really just a way to
model the idea of a starting element and a function to generate the
next element.
-}
type Source a
    = Source a (a -> a)



-- Operations for getting stuff out of sources


{-| Get the next value out of a `Source`, as well as a new
source that will return the next value after.

    -- 1
    Source.naturalNumbers
        |> Source.next
        |> Tuple.second
-}
next : Source a -> ( Source a, a )
next source =
    ( nextSource source, current source )


{-| Like `next` but it returns a list of `n` items instead.

    -- [ 1, 2, 3 ]
    Source.naturalNumbers
        |> Source.nextN 3
        |> Tuple.second
-}
nextN : Int -> Source a -> ( Source a, List a )
nextN n source =
    nextNHelper n source []


nextNHelper : Int -> Source a -> List a -> ( Source a, List a )
nextNHelper n source acc =
    if n <= 0 then
        ( source, List.reverse acc )
    else
        nextNHelper (n - 1) (nextSource source) ((current source) :: acc)


{-| Get the next value from a source without the next `Source` state.
TODO do we need peek?  maybe curent is enough

    -- 2
    Source.naturalNumbers
        |> Source.peek
-}
peek : Source a -> a
peek (Source a f) =
    f a


{-| Get the current value from a source. Kind of like `head` for a `Source`.

    -- 1
    Source.naturalNumbers
        |> Source.current
-}
current : Source a -> a
current (Source a _) =
    a


{-| Get the next state of a `Source` such that  calling `current` on that result
will return the next value that you would expect.

    -- 1
    Source.naturalNumbers
        |> Source.current

    -- 2
    Source.nextSource
        |> Source.naturalNumbers
        |> Source.current
-}
nextSource : Source a -> Source a
nextSource (Source a next) =
    Source (next a) next



-- Special sources


{-| A `Source` that returns the same thing forever.
-}
value : a -> Source a
value a =
    Source a (\a -> a)


{-| A source of natural numbers from 1 to infinity.
-}
naturalNumbers : Source Int
naturalNumbers =
    Source 1 ((+) 1)


{-| A source of tuples that represent the fibonocci numbers. This
is used by the `Stream` module and presented as values instead of tuples, but
this `Source` is what enables that.
-}
fibonocci : Source ( Int, Int )
fibonocci =
    Source ( 0, 1 ) (\( a, b ) -> ( b, a + b ))
