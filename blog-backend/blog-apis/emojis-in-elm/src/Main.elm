module Main exposing (main)


test1337 : Int
test1337 =
    3


test1338 : Int -> Int
test1338 n =
    n + 1


main : Program () Int ()
main =
    Platform.worker
        { init = \() -> ( test1337 + test1338 1, Cmd.none )
        , update = \() _ -> ( 3, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
