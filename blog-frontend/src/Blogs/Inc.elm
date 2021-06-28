module Blogs.Inc exposing
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
        , spacing
        , text
        )
import Element.Font as Font
import Element.Input as Input exposing (button)


type alias Model =
    { int : Int }


type Msg
    = Inc
    | Other


title =
    "Increment an Integer"


init : Model
init =
    { int = 0 }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Inc ->
            ( { model | int = model.int + 1 }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    column
        [ spacing 20 ]
        [ paragraph
            [ Font.size 24 ]
            [ text title ]
        , Input.button
            []
            { onPress = Just Inc
            , label = Element.text "+"
            }
        , Element.text <| String.fromInt model.int
        ]
