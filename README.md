
Elm package: http://package.elm-lang.org/packages/naddeoa/stream/latest

A `Stream` is kind of like a stream in Java 8 and kind of like a lazy list. It is a
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

## Changelog
### 2.3.0
* flatten - Convert a `Stream (Stream a) -> Stream a`

### 2.2.0
Added a bunch of async stuff, inspired from my use of rxjs. See the `Demo.elm`
file for some examples. You'll have to run `elm install` first to get
`elm-lang/html` added as a dependency so see it in `elm reactor`. I don't want
to actually depend on it and elm doesn't currently allow for dev/test
dependencies.

* StreamResult - container type for async operations
* deferNext - like the `next` function, but happens after some time
* deferNextN - like the `nextN` function, but happens after some time
* every - Subscribe to stream values until the stream is empty
