module Common.Contents exposing (..)

import Element
    exposing
        ( Element
        , el
        , link
        , newTabLink
        , paragraph
        , rgb255
        , text
        )
import Element.Background as Background
import Element.Font as Font


plainPara : String -> Element msg
plainPara str =
    paragraph [] [ text str ]


underlinedLink : String -> String -> Element msg
underlinedLink url txt =
    link
        [ Font.underline ]
        { url = url
        , label = Element.text txt
        }


underlinedLink_ : String -> Element msg
underlinedLink_ url =
    link
        [ Font.underline ]
        { url = url
        , label = text url
        }


underlinedNewTabLink : String -> String -> Element msg
underlinedNewTabLink url txt =
    newTabLink
        [ Font.underline ]
        { url = url
        , label = Element.text txt
        }


underlinedNewTabLink_ : String -> Element msg
underlinedNewTabLink_ url =
    newTabLink
        [ Font.underline ]
        { url = url
        , label = text url
        }


inlineCode : String -> Element msg
inlineCode str =
    el
        [ Background.color <| rgb255 180 180 180 ]
        (text str)
