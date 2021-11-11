module Blogs.Servant_Chat exposing
    ( Model
    , Msg
    , init
    , titleStr
    , view
    )

import Blogs.Common.Contents exposing (title)
import Common.Styles
    exposing
        ( blogViewPadding
        , paraSpacing
        )
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , width
        )


type Msg
    = Default


type alias Model =
    { a : Int }


titleStr =
    "Building a Chat Server with Haskell Servant and WebSockets"


init : Model
init =
    { a = 3 }


view : Model -> Element Msg
view model =
    column
        [ width fill
        , blogViewPadding
        , paraSpacing
        ]
        [ title titleStr
        ]
