module Blogs.Emojis_In_Elm exposing
    ( Model
    , Msg
    , init
    , titleStr
    , update
    , view
    )

import Blogs.Common.Contents
    exposing
        ( title
        , userFillableCode_
        )
import Blogs.Common.Styles exposing (commentStyle)
import Common.Colors
    exposing
        ( codeGray
        , lightBlue
        , red
        )
import Common.Contents
    exposing
        ( boldText
        , borderedButton
        , codeBlock
        , codeBlock_
        , codeBlock__
        , httpErrorView
        , inlineCode
        , italicText
        , limitedLengthInput
        , limitedLengthMultiline
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
        , roundedBorder
        , squareBorder
        )
import Common.Urls exposing (blogApisRoot)
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , onRight
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
import Elm.Compiler
import Elm.Make
import Elm.Shared
import Elm.Test
    exposing
        ( ElmTestResp(..)
        , ElmTestResult(..)
        , elmTestRespDecoder
        , elmTestResultsView
        )
import Html
import Html.Attributes as HtmlAttr
import Http
import Json.Encode as JEnc
import String.Extra as String
import Url.Builder
import Utils.Networking
    exposing
        ( httpErrorToStr
        , plainPutReq
        , utf8StringBody
        )


type alias Model =
    { unicodeToPathInput : String
    , unicodeToPathStatus : CompileStatus ElmTestResp
    , isUnicodeToPathSkipped : Bool
    , showUnicodeToPathAnswer : Bool

    --
    , firstColonPairInput_Maybe : String
    , isFirstColonPairInputTooLong_Maybe : Bool
    , firstColonPairInput_Tuple : String
    , isFirstColonPairInputTooLong_Tuple : Bool

    --
    , noColonCaseInput : String
    , notEmojiCaseInput : String
    , isEmojiCaseInput : String
    , renderStatus : CompileStatus Elm.Make.Result
    , showRenderAnswer : Bool
    , error : Maybe Http.Error
    }


type Msg
    = OnUserInputUnicodeToPath String
    | OnUserRunUnicodeToPath
    | GotRunUnicodeToPathResp (Result Http.Error ElmTestResp)
    | OnUserToggleUnicodeToPathAnswer
      --
    | OnUserInputFirstColonPair_Maybe String
    | OnUserInputFirstColonPair_Tuple String
      --
    | OnUserInputNoColonCase String
    | OnUserInputNotEmojiCase String
    | OnUserInputIsEmojiCase String
    | OnUserRender
    | OnUserToggleRenderAnswer
    | GotRenderResp (Result Http.Error Elm.Make.Result)


type CompileStatus result
    = NotStarted
    | Waiting
    | GotResult result
    | GotError Http.Error


titleStr =
    "Emojis In Elm"


init : Model
init =
    { unicodeToPathInput = ""
    , unicodeToPathStatus = NotStarted
    , isUnicodeToPathSkipped = False
    , showUnicodeToPathAnswer = False

    --
    , firstColonPairInput_Maybe = ""
    , isFirstColonPairInputTooLong_Maybe = False
    , firstColonPairInput_Tuple = ""
    , isFirstColonPairInputTooLong_Tuple = False

    --
    , noColonCaseInput = ""
    , notEmojiCaseInput = ""
    , isEmojiCaseInput = ""

    --
    , renderStatus = NotStarted
    , showRenderAnswer = False
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
            if String.length model.unicodeToPathInput > unicodeToPathInputMaxLength then
                ( model, Cmd.none )

            else
                ( { model | unicodeToPathStatus = Waiting }
                , plainPutReq
                    (Url.Builder.relative [ blogApisRoot, "emojis-in-elm", "unicode-to-path" ] [])
                    (utf8StringBody model.unicodeToPathInput)
                    (Http.expectJson GotRunUnicodeToPathResp elmTestRespDecoder)
                )

        GotRunUnicodeToPathResp result ->
            ( { model
                | unicodeToPathStatus =
                    case result of
                        Err httpError ->
                            GotError httpError

                        Ok elmTestResp ->
                            GotResult elmTestResp
              }
            , Cmd.none
            )

        OnUserToggleUnicodeToPathAnswer ->
            ( { model | showUnicodeToPathAnswer = not model.showUnicodeToPathAnswer }
            , Cmd.none
            )

        OnUserInputFirstColonPair_Maybe str ->
            if String.length str >= 10 then
                ( { model | isFirstColonPairInputTooLong_Maybe = True }, Cmd.none )

            else
                ( { model
                    | firstColonPairInput_Maybe = str
                    , isFirstColonPairInputTooLong_Maybe = False
                  }
                , Cmd.none
                )

        OnUserInputFirstColonPair_Tuple str ->
            if String.length str >= 10 then
                ( { model | isFirstColonPairInputTooLong_Tuple = True }, Cmd.none )

            else
                ( { model
                    | firstColonPairInput_Tuple = str
                    , isFirstColonPairInputTooLong_Tuple = False
                  }
                , Cmd.none
                )

        OnUserInputNoColonCase str ->
            ( { model | noColonCaseInput = str }, Cmd.none )

        OnUserInputNotEmojiCase str ->
            ( { model | notEmojiCaseInput = str }, Cmd.none )

        OnUserInputIsEmojiCase str ->
            ( { model | isEmojiCaseInput = str }, Cmd.none )

        OnUserRender ->
            if
                String.length model.noColonCaseInput
                    <= noColonCaseInputMaxLength
                    && String.length model.notEmojiCaseInput
                    <= notEmojiCaseInputMaxLength
                    && String.length model.isEmojiCaseInput
                    <= isEmojiCaseInputMaxLength
            then
                ( { model | renderStatus = Waiting }
                , -- Check if any user input has exceeded max length.
                  plainPutReq
                    (Url.Builder.relative [ blogApisRoot, "emojis-in-elm", "render" ] [])
                    (Http.jsonBody <|
                        JEnc.object
                            [ ( "noColonCase", JEnc.string model.noColonCaseInput )
                            , ( "notEmojiCase", JEnc.string model.notEmojiCaseInput )
                            , ( "isEmojiCase", JEnc.string model.isEmojiCaseInput )
                            ]
                    )
                    (Http.expectJson GotRenderResp Elm.Make.resultParser)
                )

            else
                ( model, Cmd.none )

        OnUserToggleRenderAnswer ->
            ( { model | showRenderAnswer = not model.showRenderAnswer }
            , Cmd.none
            )

        GotRenderResp result ->
            ( { model
                | renderStatus =
                    case result of
                        Err httpError ->
                            GotError httpError

                        Ok elmMakeResult ->
                            GotResult elmMakeResult
              }
            , Cmd.none
            )


view : Model -> Element Msg
view model =
    let
        isFirstColonPairInputCorrect_Maybe =
            model.firstColonPairInput_Maybe == "map2"

        isFirstColonPairInputCorrect_Tuple =
            model.firstColonPairInput_Tuple == "pair"
    in
    column
        [ width fill
        , blogViewPadding
        , paraSpacing
        ]
        [ title titleStr
        , paragraph
            commentStyle
            [ text "Before you continue: Implementing emojis in Elm is very fun! I encourage you to try it yourself first, unless you are out of idea or in a hurry. You may find new ways of implementations!"
            ]
        , paragraph
            []
            [ text "When I was building "
            , underlinedNewTabLink "https://github.com/techmindful/hideout" "Hideout"
            , text
                """, a private chat service, I figured it'd be really helpful to add an emoji feature. Texting can often cause misunderstanding"""
            , text " (proved by Key & Peele's "
            , underlinedNewTabLink "https://youtu.be/sngRrkQayDA" "skit on youtube"
            , text
                """), and adding a few emojis here and there greatly helps clarify people's intent.
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
            , text """. To get the emoji images, I had to download the entire repo. Then I went into its """
            , inlineCode "png/"
            , text
                """ 
                 directory, where emoji images of various sizes reside. I copied the directory of 32x32 version into a new directory I made under my project: 
                """
            , inlineCode "frontend/static/noto-emoji/32/"
            , text ". Note that "
            , inlineCode "frontend/"
            , text " is Elm's directory, where "
            , inlineCode "elm.json"
            , text " and "
            , inlineCode "src/"
            , text " reside. Alternatively, OpenMoji looks like a great option too: "
            , underlinedNewTabLink_ "https://github.com/hfg-gmuend/openmoji"
            , text ". This guide should be still applicable if you choose another vendor. Please comply with the licensing and copyright requirements, whichever vendor you choose."
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
        , limitedLengthInput
            unicodeToPathInputMaxLength
            [ width fill
            , Font.family [ Font.monospace ]
            ]
            { onChange = OnUserInputUnicodeToPath
            , text = model.unicodeToPathInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        , row
            [ spacing 12 ]
            [ case model.unicodeToPathStatus of
                Waiting ->
                    borderedButton Nothing "Compiling..."

                _ ->
                    borderedButton (Just OnUserRunUnicodeToPath) "Compile and Run!"
            , borderedButton (Just OnUserToggleUnicodeToPathAnswer) <|
                if model.showUnicodeToPathAnswer then
                    "Hide Answer"

                else
                    "Reveal Answer"
            ]
        , if model.showUnicodeToPathAnswer then
            column
                [ Border.width 2
                , padding 10
                , spacing 10
                , width fill
                , Font.family [ Font.monospace ]
                ]
                [ text "Answer is:"
                , plainPara "\"/static/noto-emoji/32/emoji_u\" ++ unicode ++ \".png\""
                ]

          else
            Element.none
        , unicodeToPathStatusView model.unicodeToPathStatus
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
                Turns out it isn't hard to write a recursive algorithm for this:
                """
            ]
        , codeBlock__ True <|
            """
replaceEmojis : String -> List Piece
replaceEmojis str =
    case String.indices ":" str of
        firstColonIndex :: secondColonIndex :: _ ->
            let
                possibleEmojiName =
                    String.slice (firstColonIndex + 1) secondColonIndex str

                -- TODO: Implement the check later.
                isEmoji =
                    True
            in
            -- Has colon of pairs, but it doesn't match an emoji name.
            if not isEmoji then
                (Text <| String.left secondColonIndex str)
                    :: (replaceEmojis <| String.dropLeft secondColonIndex str)"""
                ++ """

            -- Found an emoji
            else
"""
                ++ (indentMultiline 16 <| userFillableCode_ model.isEmojiCaseInput)
                ++ """
                ++ (replaceEmojis <| String.dropLeft (secondColonIndex + 1) str)

        -- No pair of colons. No emojis.
        _ ->
            """
                ++ userFillableCode_ model.noColonCaseInput
        , paragraph
            []
            [ text
                """
                The pure function first tries to find two colons and their indices in the string, using
                """
            , inlineCode "String.indices"
            , text " from "
            , underlinedNewTabLink
                "https://package.elm-lang.org/packages/elm/core/latest/String#indices"
                "elm/core"
            , text
                """. If it can't find at least 2 colons, that means there definitely isn't any emoji in the string, and the whole string should just be parsed as 
                """
            , inlineCode "Text"
            , text ". Complete what to return in this case (the ??? at the last line):"
            ]
        , limitedLengthInput
            noColonCaseInputMaxLength
            [ width fill
            , Font.family [ Font.monospace ]
            ]
            { onChange = OnUserInputNoColonCase
            , text = model.noColonCaseInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        , paragraph
            []
            [ text
                """
                If it finds two colons, it slices out the substring in-between, and checks if it's an emoji unicode. We'll implement the checking later. But notice how we have the opportunity to implement the check, in contrast to using regex!
                """
            ]
        , paragraph
            []
            [ text
                """
                If the substring in-between is not an emoji, then everything before the second colon should be parsed as 
                """
            , inlineCode "Text"
            , text
                """. Be careful not to include the second colon here. It could be a part of a following emoji! The code for what 
                """
            , inlineCode "Piece"
            , text "s to return is already given. Notice how it uses a recursive call to parse the rest of the string, starting at the second colon."
            ]
        , paragraph
            []
            [ text
                """
            And if the substring between colon is an emoji unicode, then everything before the 
            """
            , italicText "first"
            , text " colon should be parsed as "
            , inlineCode "Text"
            , text ", the unicode and the surrounding colons should be parsed as "
            , inlineCode "Emoji"
            , text ". Just like the previous not-emoji case, the rest of the string will be parsed into a list of "
            , inlineCode "Piece"
            , text
                """s by a recursive call to the function itself. The result is the whole thing concatenated.
                """
            ]
        , paragraph
            []
            [ text "So what are the result "
            , inlineCode "Piece"
            , text "s before the recursive call?"
            ]
        , limitedLengthMultiline
            isEmojiCaseInputMaxLength
            [ width fill
            , height <| Element.minimum 98 fill
            , Font.family [ Font.monospace ]
            ]
            { onChange = OnUserInputIsEmojiCase
            , text = model.isEmojiCaseInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            , spellcheck = False
            }
        , row
            [ spacing 15 ]
            [ case model.renderStatus of
                Waiting ->
                    borderedButton Nothing "Compiling..."

                _ ->
                    borderedButton (Just OnUserRender) "Compile and Run!"
            , borderedButton (Just OnUserToggleRenderAnswer) <|
                if model.showRenderAnswer then
                    "Hide Answer"

                else
                    "Reveal Answer"
            ]
        , if model.showRenderAnswer then
            codeBlock__ True
                """
-- Here are the answers.

-- No colon case:
[ Text str ]

-- Found emoji case:
[ Text <| String.left firstColonIndex str
, Emoji possibleEmojiName
]
                        """

          else
            Element.none
        , case model.renderStatus of
            GotError httpError ->
                httpErrorView httpError

            GotResult (Elm.Make.CompilerError str) ->
                el
                    (squareBorder 10
                        ++ [ width fill ]
                    )
                    (Elm.Compiler.errorView str)

            GotResult (Elm.Make.Html str) ->
                el
                    [ width fill ]
                <|
                    Element.html <|
                        Html.iframe
                            [ HtmlAttr.srcdoc <| str
                            , HtmlAttr.style "height" "900px"
                            ]
                            []

            NotStarted ->
                el
                    (squareBorder 10
                        ++ [ width fill
                           , Font.family [ Font.monospace ]
                           ]
                    )
                    (text "Run the code and result will be displayed.")

            Waiting ->
                el
                    (squareBorder 10
                        ++ [ width fill
                           , Font.family [ Font.monospace ]
                           ]
                    )
                    (text "Compiling...")
        , plainPara
            """
            If your results match the expected, then congrats! We are now parsing emojis in a piece of text. Except that we haven't implemented the vital check of whether the string between a colon pair is meant for an emoji. Not doing so can throw off the parsing of the whole string, as demonstrated earlier. This will be included in the next part of the guide, which I'll publish once it's completed.
            """
        ]


unicodeToPathStatusView : CompileStatus ElmTestResp -> Element msg
unicodeToPathStatusView status =
    el
        [ Border.width 2
        , padding 10
        , width fill
        , Font.family [ Font.monospace ]
        ]
    <|
        case status of
            GotError httpError ->
                column
                    [ width fill
                    , spacing 15
                    ]
                    [ el
                        [ Border.widthEach { edges | bottom = 2 }
                        , paddingEach { edges | bottom = 5 }
                        , width fill
                        ]
                        (el
                            [ Font.size 24
                            , Font.color red
                            ]
                            (text "Networking Error")
                        )
                    , paragraph
                        [ width fill ]
                        [ text <| httpErrorToStr httpError ]
                    ]

            GotResult elmTestResp ->
                case elmTestResp of
                    CompilerError_ errorMsg ->
                        Elm.Compiler.errorView errorMsg

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
                                        [ Element.html <|
                                            Html.p
                                                [ HtmlAttr.style "white-space" "pre-wrap"
                                                , HtmlAttr.style "overflow-wrap" "anywhere"
                                                , HtmlAttr.style "margin" "0 auto"
                                                ]
                                                [ Html.text <| "Actual  : " ++ failure.actual ]
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
                        Elm.Test.internalServerErrorView

            NotStarted ->
                text "Run the code and result will be displayed."

            Waiting ->
                text "Compiling..."


indentMultiline : Int -> String -> String
indentMultiline numSpaces str =
    let
        indentLine : String -> String
        indentLine line =
            String.repeat numSpaces " " ++ line
    in
    str
        |> String.lines
        |> List.map indentLine
        |> String.join "\n"


unicodeToPathInputMaxLength =
    99


noColonCaseInputMaxLength =
    39


notEmojiCaseInputMaxLength =
    99


isEmojiCaseInputMaxLength =
    119


grinEmojiPath =
    "/static/noto-emoji/32/emoji_u1f600.png"


robotEmojiPath =
    "/static/noto-emoji/32/emoji_u1f916.png"


heartEmojiPath =
    "/static/noto-emoji/32/emoji_u2764.png"
