module Blogs.Emojis_Elm exposing
    ( Model
    , Msg
    , init
    , title
    , update
    , view
    )

import Common.Contents
    exposing
        ( inlineCode
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
        , paddingXY
        , paragraph
        , spacingXY
        , text
        )
import Element.Font as Font
import Element.Input as Input


type alias Model =
    { codeInput : String }


type Msg
    = UserInputCode String
    | Other


title =
    "Emojis In Elm"


init : Model
init =
    { codeInput = "" }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UserInputCode str ->
            ( { model | codeInput = str }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column
        [ blogViewPadding
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
            , inlineCode "static/emojis/32/"
            , text ". I also copied the LICENSE file and put it under "
            , inlineCode "static/emojis/"
            , text """, in order to fulfill the licensing obligations. Alternatively, OpenMoji looks like a great option too: """
            , underlinedNewTabLink_ "https://github.com/hfg-gmuend/openmoji"
            , text ". This guide should be still applicable if you choose another vendor."
            ]
        ]
