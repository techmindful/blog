module Elm.Make exposing (..)

import Json.Decode exposing (Decoder, field, map, oneOf, string, succeed)


type Result
    = CompilerError String
    | Html String


resultParser : Decoder Result
resultParser =
    oneOf
        [ map CompilerError <| field "compilerError" string
        , map Html <| field "html" string
        ]
