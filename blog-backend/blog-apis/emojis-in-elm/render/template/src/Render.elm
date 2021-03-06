module Render exposing (..)

import Browser
import Element
    exposing
        ( Element
        , column
        , image
        , padding
        , paragraph
        , row
        , spacing
        , text
        )
import Element.Font as Font
import Html exposing (Html)
import List.Extra as List
import String.Extra as String


type Piece
    = Text String
    | Emoji String


unicodeToPath : String -> String
unicodeToPath unicode =
    "/static/noto-emoji/32/emoji_u" ++ unicode ++ ".png"


replaceEmojis : String -> List Piece
replaceEmojis str =
    case String.indices ":" str of
        firstColonIndex :: secondColonIndex :: _ ->
            let
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
                -- Insert isEmojiCase here.
                ++ (replaceEmojis <| String.dropLeft (secondColonIndex + 1) str)

        -- No pair of colons. No emojis.
        _ ->
            -- Insert noColonCase here.


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
    let
        targetStrings : List String
        targetStrings =
            [ "No colon in this sentence."
            , "One colon : here."
            , "One colon: here."
            , ":One colon at the start."
            , "One colon at the end:"
            , ":1f600:"
            , "Hello :1f600:, nice to meet you. I :2764: coding :1f916:"
            ]

        targetStringView : String -> Element msg
        targetStringView target =
            column
                [ spacing 10 ]
                [ text <| "Target   string - " ++ target
                , row [] <|
                    [ text "Expected result - " ]
                        ++ (List.map renderPiece <| correctReplaceEmojis target)

                , row [] <|
                    [ text "Actual   result - " ]
                        ++ (List.map renderPiece <| replaceEmojis target)
                ]
    in
    Element.layout
        []
    <|
        column
            [ padding 10
            , spacing 40
            , Font.family [ Font.monospace ]
            ]
            (List.map targetStringView targetStrings)


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \() () -> ()
        , view = view
        }


correctReplaceEmojis : String -> List Piece
correctReplaceEmojis str =
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

        Just ( firstColonIndex, secondColonIndex ) ->
            let
                possibleEmojiName =
                    String.slice (firstColonIndex + 1) secondColonIndex str

                isEmoji =
                    True
            in
            -- Has colon of pairs, but it doesn't match an emoji name.
            if not isEmoji then
                (Text <| String.left secondColonIndex str)
                    :: (correctReplaceEmojis <| String.dropLeft secondColonIndex str)

            else
                -- Found an emoji
                [ Text <| String.left firstColonIndex str
                , Emoji possibleEmojiName
                ]
                    ++ (correctReplaceEmojis <| String.dropLeft (secondColonIndex + 1) str)
