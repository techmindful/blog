module Render exposing (..)

import Browser
import Element
    exposing
        ( Element
        , column
        , image
        , paragraph
        , text
        )
import Html exposing (Html)
import List.Extra as List


type Piece
    = Text String
    | Emoji String


unicodeToPath : String -> String
unicodeToPath unicode =
    -- No leading / because the html is opened with file://
    "../../../blog-frontend/static/noto-emoji/32/emoji_u" ++ unicode ++ ".png"


replaceEmojis : String -> List Piece
replaceEmojis str =
    let
        colonIndices : List Int
        colonIndices =
            String.indices ":" str

        firstColonPair : Maybe ( Int, Int )
        firstColonPair =
            Maybe.map2 Tuple.pair
                (List.getAt 0 colonIndices)
                (List.getAt 1 colonIndices)
    in
    case firstColonPair of
        -- No pair of colons. No emojis.
        Nothing ->
            [ Text str ]

        Just pair ->
            let
                firstColonIndex =
                    Tuple.first pair

                secondColonIndex =
                    Tuple.second pair

                possibleEmojiName =
                    String.slice (firstColonIndex + 1) secondColonIndex str

                isEmoji =
                    True
            in
            -- Has colon of pairs, but it doesn't match an emoji name.
            if not isEmoji then
                (Text <| String.left secondColonIndex str)
                    :: (replaceEmojis <| String.dropLeft secondColonIndex str)

            else
                -- Found an emoji
                [ Text <| String.left firstColonIndex str
                , Emoji possibleEmojiName
                ]
                    ++ (replaceEmojis <| String.dropLeft (secondColonIndex + 1) str)


renderPiece : Piece -> Element msg
renderPiece piece =
    case piece of
        Text str ->
            text str

        Emoji name ->
            image
                []
                { src = unicodeToPath name
                , description = ""
                }


view : () -> Html ()
view _ =
    Element.layout
        []
    <|
        paragraph
            []
            (List.map renderPiece <|
                replaceEmojis "Hello :1f600:, nice to meet you. I :2764: coding :1f916:"
            )


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \() () -> ()
        , view = view
        }
