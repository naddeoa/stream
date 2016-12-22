port module Main exposing (..)

import StreamTests
import SourceTests
import Test exposing (Test)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


tests : Test
tests =
    Test.concat [ StreamTests.all, SourceTests.all ]


main : Test.Runner.Node.TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
