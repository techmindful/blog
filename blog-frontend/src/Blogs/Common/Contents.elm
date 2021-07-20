module Blogs.Common.Contents exposing
    ( UserCodePlaceholder
    , userFillableCode_
    )

{-| Common _blog_ contents.
-}


type UserCodePlaceholder
    = UserCodePlaceholder String


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
