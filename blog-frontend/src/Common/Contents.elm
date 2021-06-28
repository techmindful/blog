module Common.Contents exposing (..)

import Element
    exposing
        ( Element
        , paragraph
        , text
        )


plainPara : String -> Element msg
plainPara str =
    paragraph [] [ text str ]
