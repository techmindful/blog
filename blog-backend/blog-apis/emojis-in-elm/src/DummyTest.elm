module DummyTest exposing (..)

import Expect
import Test
    exposing
        ( Test
        , describe
        , test
        )


suite : Test
suite =
    describe "Dummies"
        [ test "dummy" (\_ -> Expect.equal 0 (List.length []))
        , test "dummy2" (\_ -> Expect.equal 1 (List.length []))
        ]
