module FirstColonPair exposing (suite)

import Expect
import Test exposing (Test, describe, test)


getFirstColonPair : String -> Maybe ( Int, Int )
getFirstColonPair str =
    let
        colonIndices : List Int
        colonIndices =
            String.indices ":" str

        firstColonPair : Maybe ( Int, Int )
        firstColonPair =
            Maybe.map2 Tuple.pair
                (List.getAt 0 colonIndices)
                (List.getAt 1 colonIndices)
    in
    firstColonPair


suite : Test
suite =
    let
        mkTest : String -> Test
        mkTest unicode =
            test
                ("Testing emoji of unicode " ++ unicode)
                (\() ->
                    userUnicodeToPath unicode
                        |> Expect.equal (correctUnicodeToPath unicode)
                )
    in
    describe
        "Translate emoji unicode to the corresponding image file path."
        [ mkTest "1f600"
        , mkTest "1f916"
        , mkTest "2764"
        ]
