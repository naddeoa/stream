module Stream.Source
    exposing
        ( Source
        , take
        , taken
        , peek
        , singleton
        , naturalNumbers
        , fibonocci
        , current
        )


type Source a
    = Source a (a -> a)



-- Operations for getting stuff out of sources


take : Source a -> ( Source a, a )
take source =
    ( nextSource source, peek source )


taken : Int -> Source a -> ( Source a, List a )
taken n source =
    takenHelper n source []


takenHelper : Int -> Source a -> List a -> ( Source a, List a )
takenHelper n source acc =
    if n <= 0 then
        ( source, List.reverse acc )
    else
        takenHelper (n - 1) (nextSource source) ((peek source) :: acc)


peek : Source a -> a
peek (Source a next) =
    next a


current : Source a -> a
current (Source a next) =
    a


nextSource : Source a -> Source a
nextSource (Source a next) =
    Source (next a) next



-- Utility functions


identity : a -> a
identity a =
    a



-- Special sources


singleton : a -> Source a
singleton a =
    Source a identity


naturalNumbers : Source Int
naturalNumbers =
    Source 0 ((+) 1)


fibonocci : Source ( Int, Int )
fibonocci =
    Source ( 0, 1 ) (\( a, b ) -> ( b, a + b ))
