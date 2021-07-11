module UnicodeToPath exposing (suite)

import Expect
import Test exposing (Test, describe, test)


correctUnicodeToPath : String -> String
correctUnicodeToPath unicode =
    "/static/noto-emoji/32/emoji_u" ++ unicode ++ ".png"


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


userUnicodeToPath : String -> String
userUnicodeToPath unicode =
    
