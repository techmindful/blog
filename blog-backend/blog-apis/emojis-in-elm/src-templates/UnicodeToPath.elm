module UnicodeToPath exposing (suite)

import Expect
import Test exposing (Test, test)


correctUnicodeToPath : String -> String
correctUnicodeToPath unicode =
    "/static/noto-emoji/32/emoji_u" ++ unicode ++ ".png"


suite : Test
suite =
    test
        "Emoji unicode matches the image file path."
        (\_ ->
            userUnicodeToPath "1f600"
                |> Expect.equal (correctUnicodeToPath "1f600")
        )


userUnicodeToPath : String -> String
userUnicodeToPath unicode =
    
