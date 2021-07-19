module Elm.Shared exposing (title)

import Common.Contents exposing (sizedText)
import Element
    exposing
        ( Element
        , el
        , fill
        , paddingEach
        , width
        )
import Element.Border as Border


title : String -> Element msg
title str =
    el
        [ Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
        , paddingEach { bottom = 5, top = 0, left = 0, right = 0 }
        , width fill
        ]
        (sizedText 24 str)
