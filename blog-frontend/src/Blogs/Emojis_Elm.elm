module Blogs.Emojis_Elm exposing
    ( Model
    , Msg
    , init
    , title
    , update
    , view
    )

import Common.Contents exposing (plainPara)
import Element
    exposing
        ( Element
        , column
        , paragraph
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
    "Parsing and Displaying Emojis Alongside Markdown in Elm"


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
        []
        [ paragraph
            [ Font.size 24 ]
            [ text title ]
        , Input.multiline
            []
            { onChange = UserInputCode
            , text = model.codeInput
            , placeholder = Nothing
            , label = Input.labelHidden ""
            , spellcheck = False
            }
        ]
