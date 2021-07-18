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
        ( ElmTestResp(..)
        , ElmTestResult(..)
        , elmTestCompilerErrorView
        , elmTestRespDecoder
        , elmTestResultsView
        )
import Common.Colors exposing (codeGray)
import Common.Contents
    exposing
        ( boldText
        , borderedButton
        , codeBlock
        , codeBlock_
        , codeBlock__
        , inlineCode
        , italicText
        , plainImage
        , plainPara
        , underlinedNewTabLink
        , underlinedNewTabLink_
        , wordBreakPara
        )
import Common.Styles
    exposing
        ( blogViewPadding
        , edges
        , paraSpacing
        , squareBorder
        )
import Common.Urls exposing (blogApisRoot)
import Element
    exposing
        ( Element
        , alignRight
        , column
        , el
        , fill
        , image
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , row
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html
import Html.Attributes as HtmlAttr
import Http
import Markdown
import String.Extra as String
import Url.Builder
import Utils.Networking exposing (plainPutReq, utf8StringBody)


type alias Model =
    { unicodeToPathInput : String
    , unicodeToPathResp : Maybe ElmTestResp
    , isUnicodeToPathSkipped : Bool
    , firstColonPairInput : String
    , renderResult : Result Http.Error String
    , error : Maybe Http.Error
    }


type Msg
    = OnUserInputUnicodeToPath String
    | OnUserRunUnicodeToPath
    | GotRunUnicodeToPathResp (Result Http.Error ElmTestResp)
    | OnUserInputFirstColonPair String
    | OnUserRender
    | GotRenderResp (Result Http.Error String)


title =
    "Emojis In Elm"


init : Model
init =
    { unicodeToPathInput = ""
    , unicodeToPathResp = Nothing
    , isUnicodeToPathSkipped = False
    , firstColonPairInput = ""
    , renderResult = Ok ""
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
            , plainPutReq
                (Url.Builder.relative [ blogApisRoot, "emojis-in-elm", "unicode-to-path" ] [])
                (utf8StringBody model.unicodeToPathInput)
                (Http.expectJson GotRunUnicodeToPathResp elmTestRespDecoder)
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

        OnUserInputFirstColonPair str ->
            ( { model | firstColonPairInput = str }
            , Cmd.none
            )

        OnUserRender ->
            ( model
            , Http.get
                { url = Url.Builder.relative [ blogApisRoot, "emojis-in-elm", "render" ] []
                , expect = Http.expectString GotRenderResp
                }
              --, plainPutReq
              --    (Url.Builder.relative [ blogApisRoot, "emojis-in-elm", "render" ] [])
              --    (utf8StringBody model.firstCol)
              --    (Http.expectString GotRenderResp)
            )

        GotRenderResp result ->
            ( { model | renderResult = result }
            , Cmd.none
            )


view : Model -> Element Msg
view model =
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
        , el
            [ Border.width 2
            , padding 10
            , width fill
            ]
            (unicodeToPathRespView model.unicodeToPathResp)
        , Input.text
            [ width fill ]
            { onChange = OnUserInputUnicodeToPath
            , text = model.unicodeToPathInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        , borderedButton OnUserRunUnicodeToPath "Compile and Run!"
        , paragraph
            []
            [ text "Now that we can find the image based on the unicode the user has entered like "
            , inlineCode ":1f600:"
            , text ", let's parse the string input by user into a list of "
            , inlineCode "Piece"
            , text "s. For example, if user entered a string "
            ]
        , paragraph
            (squareBorder 10 ++ [ Background.color codeGray ])
            [ text "\"Hello :1f600:, nice to meet you. I :2764: coding :1f916:\"" ]
        , plainPara "It should be parsed into:"
        , codeBlock__ False
            """
[ Text "Hello "
, Emoji "1f600"
, Text ", nice to meet you. I "
, Emoji "2764"
, Text " coding "
, Emoji "1f916"
]
            """
        , paragraph
            []
            [ text "Each "
            , inlineCode "Piece"
            , text " is then rendered based on if it's a "
            , inlineCode "Text"
            , text " or "
            , inlineCode "Emoji"
            , text ". The rendered products will be put together in a row, resulting in something like: "
            ]
        , row
            (squareBorder 10)
            [ text "Hello "
            , plainImage grinEmojiPath "grin emoji"
            , text ", nice to meet you. I "
            , plainImage heartEmojiPath "heart emoji"
            , text " coding "
            , plainImage robotEmojiPath "robot emoji"
            ]
        , paragraph
            []
            [ text "The first method I tried was using "
            , inlineCode "elm/parser"
            , text
                """. However, after the parser I wrote made the page freeze with what looks like an infinite loop, and the compiler couldn't be there for me, I gave up and switched to try """
            , inlineCode "elm/regex"
            , text ". It almost worked. With the regex "
            , inlineCode ":[^:]+:"
            , text ", I could find and replace "
            , italicText "every other pair"
            , text " of colons and the content in-between. However, it can't handle strings like: "
            ]
        , paragraph
            (squareBorder 10 ++ [ Background.color codeGray ])
            [ text "\"Alice said: Hello :1f600:, nice to meet you.\"" ]
        , paragraph
            []
            [ text "In this case, the first pair of colons is "
            , inlineCode ": Hello :"
            , text ". My code would end up trying to replace "
            , inlineCode ": Hello :"
            , text " with an emoji image whose unicode is "
            , inlineCode " Hello "
            , text ", and fail to match the colon pair "
            , inlineCode ":1f600:"
            , text " that is actually intended as an emoji, "
            , text
                """
                So I had to write my own algorithm to match only colon pairs that contains a 
                """
            , italicText "valid"
            , text " emoji unicode in-between."
            ]
        , paragraph
            []
            [ text
                """
                Turns out it isn't hard to write a recursive algorithm for this. The function first tries to locate a pair of colons in the input string. If it can't find at least 2 colons, that means there definitely isn't any emoji in the string, and it should just be parsed as 
                """
            , inlineCode "Text"
            , text
                """. If it finds two colons, it slices out the substring in-between, and checks if it's an emoji unicode. If it is not, then everything before the second colon should be parsed as 
                """
            , inlineCode "Text"
            , text
                """. Be careful not to include the second colon here. It could be a part of a following emoji! And if the substring between colon is an emoji unicode, then everything before the 
                """
            , italicText "first"
            , text " colon should be parsed as "
            , inlineCode "Text"
            , text ", the unicode and the surrounding colons should be parsed as "
            , inlineCode "Emoji"
            , text ". In both cases, the rest of the string will be parsed into a list of "
            , inlineCode "Piece"
            , text
                """s by a recursive call to the function itself. The result is the whole thing concatenated.
                """
            ]
        , codeBlock__ True <|
            """
replaceEmojis : String -> List Piece
replaceEmojis str =
    let
        colonIndices : List Int
        colonIndices =
            String.indices ":" str

        firstColonPair : Maybe ( Int, Int )
        firstColonPair =
            """
                ++ model.firstColonPairInput
                ++ """
                (List.getAt 0 colonIndices)
                (List.getAt 1 colonIndices)
                    """
        , paragraph
            []
            [ inlineCode "colonIndices"
            , text
                """ locates the index of each colon character in the string. 
                """
            , inlineCode "firstColonPair"
            , text " uses "
            , inlineCode "List.getAt"
            , text " from "
            , underlinedNewTabLink
                "https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#getAt"
                "elm-community/list-extra"
            , text
                """ to attempt to get the indices of first and second colons. Can you complete the first line which will combine the two Maybe indices into a Maybe tuple?
                """
            ]
        , Input.text
            [ width fill ]
            { onChange = OnUserInputFirstColonPair
            , text = model.firstColonPairInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        , borderedButton OnUserRender "Compile and Run!"
        , el
            [ width fill ]
          <|
            Element.html <|
                Html.iframe
                    [ HtmlAttr.srcdoc <|
                        case model.renderResult of
                            Err _ ->
                                "HTTP Error"

                            Ok str ->
                                str
                    ]
                    []
        ]


unicodeToPathRespView : Maybe ElmTestResp -> Element msg
unicodeToPathRespView maybeResp =
    case maybeResp of
        Nothing ->
            BackendElmResp.noRespView_

        Just resp ->
            case resp of
                CompilerError_ errorMsg ->
                    elmTestCompilerErrorView errorMsg

                Results results ->
                    let
                        mkEmoji : String -> Element msg
                        mkEmoji src =
                            el
                                [ alignRight
                                , paddingEach { edges | right = 20 }
                                ]
                            <|
                                image
                                    [ Border.widthEach { edges | left = 2 }
                                    , paddingEach { edges | left = 10 }
                                    ]
                                    { src = src
                                    , description = ""
                                    }
                    in
                    elmTestResultsView
                        results
                        [ String.quote grinEmojiPath
                        , String.quote robotEmojiPath
                        , String.quote heartEmojiPath
                        ]
                        (\failure ->
                            column
                                [ width fill
                                , spacing 10
                                ]
                                [ row
                                    [ width fill ]
                                    [ plainPara <| "Expected: " ++ failure.expected
                                    , mkEmoji <| String.unquote failure.expected
                                    ]
                                , row
                                    [ width fill ]
                                    [ wordBreakPara <| "Actual: " ++ failure.actual
                                    , mkEmoji <| String.unquote failure.actual
                                    ]
                                ]
                        )
                        (\expected ->
                            row
                                [ width fill ]
                                [ plainPara <| "Passed: " ++ expected
                                , mkEmoji <| String.unquote expected
                                ]
                        )

                InternalServerError ->
                    BackendElmResp.internalServerErrorView


grinEmojiPath =
    "/static/noto-emoji/32/emoji_u1f600.png"


robotEmojiPath =
    "/static/noto-emoji/32/emoji_u1f916.png"


heartEmojiPath =
    "/static/noto-emoji/32/emoji_u2764.png"
