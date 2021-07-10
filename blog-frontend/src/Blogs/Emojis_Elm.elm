module Blogs.Emojis_Elm exposing
    ( Model
    , Msg
    , init
    , title
    , update
    , view
    )

import BackendElmResp
    exposing
        ( ElmTestResp
        , elmTestRespDecoder
        , elmTestRespView
        )
import Common.Contents
    exposing
        ( borderedButton
        , codeBlock
        , codeBlock_
        , codeBlock__
        , inlineCode
        , plainPara
        , underlinedNewTabLink
        , underlinedNewTabLink_
        )
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
        , image
        , padding
        , paddingXY
        , paragraph
        , spacingXY
        , text
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html
import Http
import Markdown
import String.Extra as String


type alias Model =
    { unicodeToPathInput : String
    , unicodeToPathResp : Maybe ElmTestResp
    , isUnicodeToPathSkipped : Bool
    , error : Maybe Http.Error
    }


type Msg
    = OnUserInputUnicodeToPath String
    | OnUserRunUnicodeToPath
    | GotRunUnicodeToPathResp (Result Http.Error ElmTestResp)


title =
    "Emojis In Elm"


init : Model
init =
    { unicodeToPathInput = ""
    , unicodeToPathResp = Nothing
    , isUnicodeToPathSkipped = False
    , error = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUserInputUnicodeToPath str ->
            ( { model | unicodeToPathInput = str }
            , Cmd.none
            )

        OnUserRunUnicodeToPath ->
            ( model
            , Http.request
                { method = "PUT"
                , headers = []
                , url = "/blog-apis/emojis-in-elm/unicode-to-path/"
                , body = Http.stringBody "text/plain;charset=utf-8" model.unicodeToPathInput
                , expect = Http.expectJson GotRunUnicodeToPathResp elmTestRespDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotRunUnicodeToPathResp result ->
            case result of
                Err httpError ->
                    ( { model | error = Just httpError }
                    , Cmd.none
                    )

                Ok resp ->
                    ( { model | unicodeToPathResp = Just resp }
                    , Cmd.none
                    )


view : Model -> Element Msg
view model =
    let
        grinEmojiPath : String
        grinEmojiPath =
            "/static/noto-emoji/32/emoji_u1f600.png"
    in
    column
        [ width fill
        , blogViewPadding
        , paraSpacing
        ]
        [ paragraph
            [ Font.size 32 ]
            [ text title ]
        , paragraph
            []
            [ text "When I was building "
            , underlinedNewTabLink "https://github.com/techmindful/hideout" "Hideout"
            , text
                """, a private chat service, I figured it'd be really helpful to add an emoji feature.
                """
            , underlinedNewTabLink
                "https://www.youtube.com/watch?v=naleynXS7yo"
                "Texting can often cause misunderstanding"
            , text
                """, and adding a few emojis here and there greatly helps clarify people's intent.
                """
            ]
        , paragraph
            []
            [ text
                """
                However, despite its usefulness, I was surprised that I couldn't find an Elm package as an existing solution. There didn't seem to be any guide written about handling emojis either. So after having implemented it, I think it may be helpful to author a guide on the topic.
                """
            ]
        , paragraph
            []
            [ text
                """
                First we need to get the emoji images. Google's Noto Emoji seems like an okay option: 
                """
            , underlinedNewTabLink_ "https://github.com/googlefonts/noto-emoji"
            , text """. To get the emoji images, I had to download the entire repo. Then I went into the png folder, where emoji images of various sizes reside. I copied the folder of 32x32 version into a new folder I made under my project: 
                """
            , inlineCode "static/noto-emoji/32/"
            , text ". I also copied the LICENSE file and put it under "
            , inlineCode "static/noto-emoji/"
            , text """, to fulfill the licensing obligations. Alternatively, OpenMoji looks like a great option too: """
            , underlinedNewTabLink_ "https://github.com/hfg-gmuend/openmoji"
            , text ". This guide should be still applicable if you choose another vendor."
            ]
        , paragraph
            []
            [ text
                """
                To allow emojis in user's input, let's define a new syntax. User can indicate their intention to use an emoji by enclosing a unique identifier for the emoji between two colons, for example 
                """
            , inlineCode ":amused:"
            , text """. This is a common method among popular messaging apps like Element, Slack, Discord, etc. We haven't mapped the descriptive names like "amused" to each emoji yet. But if we take a look at the file names of the emoji images, we can see that they are all in the format of 
                """
            , inlineCode "emoji_<unicode>.png"
            , text """. So wherever user's input string contains a pair of colons, we can try to see if the text in-between matches any emoji unicode, and substitute the piece with an emoji image. We can write a pure function for this! It'll take a string, and convert it into a list of "pieces". A piece is either text, or emoji. Let's define a custom type for it:
                """
            ]
        , el
            [ width fill ]
          <|
            codeBlock__ False
                """
type Piece
    = Text String
    | Emoji String
            """
        , paragraph
            []
            [ text "The "
            , inlineCode "String"
            , text " in "
            , inlineCode "Emoji String"
            , text " is the supposed emoji unicode, excluding colons. For example, "
            , inlineCode "Emoji \"1f600\""
            , text " should be rendered as an emoji image located at "
            , inlineCode grinEmojiPath
            , text ", which is "
            , image
                []
                { src = grinEmojiPath
                , description = "Emoji of unicode 1f600"
                }
            , text "."
            , text
                """
                Can you complete the function to translate an emoji unicode to the file path of its image?
                """
            ]
        , el
            [ width fill ]
          <|
            codeBlock_ True <|
                """
unicodeToPath : String -> String
unicodeToPath unicode =
    """
                    ++ model.unicodeToPathInput
        , elmTestRespView
            model.unicodeToPathResp
            (String.quote grinEmojiPath)
        , Input.text
            [ width fill ]
            { onChange = OnUserInputUnicodeToPath
            , text = model.unicodeToPathInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        , borderedButton OnUserRunUnicodeToPath "Compile and Run!"
        ]
