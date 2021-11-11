module Blogs.Common.Contents exposing
    ( UserCodePlaceholder
    , title
    , userFillableCode_
    )

{-| Common _blog_ contents.
-}

import Element
    exposing
        ( Element
        , paragraph
        , text
        )
import Element.Font as Font


type UserCodePlaceholder
    = UserCodePlaceholder String


title : String -> Element msg
title str =
    paragraph
        [ Font.size 32 ]
        [ text str ]


{-| A part of code which is supposed to be input by user.
When user hasn't given input, it will show as some placeholder string.
-}
userFillableCode : UserCodePlaceholder -> String -> String
userFillableCode (UserCodePlaceholder placeholder) input =
    if String.isEmpty input then
        placeholder

    else
        input


userFillableCode_ : String -> String
userFillableCode_ =
    userFillableCode (UserCodePlaceholder "???")
