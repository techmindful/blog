module Routing exposing
    ( Route(..)
    , getRoute
    )

import Url exposing (Url)
import Url.Parser
    exposing
        ( (</>)
        , Parser
        , map
        , oneOf
        , parse
        , s
        , string
        , top
        )


type Route
    = Root
    | Blog String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Root top
        , map Blog <| s "blog" </> string
        ]


getRoute : Url -> Route
getRoute url =
    Maybe.withDefault NotFound <| parse routeParser url
