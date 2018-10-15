
This branch is a demo of an issue I'm having. The same code doesn't compile
under elm 19 becuase of a type error. Reproduce as follows.


```bash
git clone https://github.com/naddeoa/stream.git
cd stream

# building with elm 18 works
npm i elm@0.18
npx elm make


# building with elm 19 breaks
npm i elm@0.19
npx elm make
```
Building with elm will give the following error. I don't know if this is a bug
in 18 or in 19 so I'm not sure how best to fix it yet.

```
-- TYPE MISMATCH ------------------------------------------------ src/Stream.elm                                                                                                                                                     [200/1869]

The 1st argument to `FlattenStream` is not what I expect:

594|                                     next (FlattenStream ( nextNestedStreams, n ))
                                                             ^^^^^^^^^^^^^^^^^^^^^^^^
This argument is a tuple of type:

    ( Stream b, b )

But `FlattenStream` needs the 1st argument to be:

    ( Stream (Stream b), Stream b )

Hint: Your type annotation uses type variable `b` which means ANY type of value
can flow through, but your code is saying it specifically wants a `Stream`
value. Maybe change your type annotation to be more specific? Maybe change the
code to be more general?

Read <https://elm-lang.org/0.19.0/type-annotations> for more advice!

-- TYPE MISMATCH ------------------------------------------------ src/Stream.elm

The 1st argument to `next` is not what I expect:

590|                                 next nestedStreams
                                          ^^^^^^^^^^^^^
This `nestedStreams` value is a:

    Stream (Stream b)

But `next` needs the 1st argument to be:

    Stream b

Hint: Your type annotation uses type variable `b` which means ANY type of value
can flow through, but your code is saying it specifically wants a `Stream`
value. Maybe change your type annotation to be more specific? Maybe change the
code to be more general?

```
