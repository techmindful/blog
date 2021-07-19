module Main exposing (..)

import Blogs.Index
import Blogs.Types
import Browser
import Browser.Navigation as Nav
import Common.Colors exposing (..)
import Common.Contents as Utils exposing (plainPara)
import Common.Styles exposing (..)
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , onLeft
        , padding
        , paddingXY
        , paragraph
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input exposing (button)
import Routing exposing (Route(..), getRoute)
import Url exposing (Url)
import Url.Parser


type alias Model =
    { route : Route
    , navKey : Nav.Key
    , blogModels : Blogs.Types.Models
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | BlogMsg Blogs.Types.Msg
    | Nop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { route = getRoute url
      , navKey = navKey
      , blogModels = Blogs.Types.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                Browser.External urlStr ->
                    ( model, Nav.load urlStr )

        UrlChanged url ->
            ( { model | route = getRoute url }, Cmd.none )

        BlogMsg blogMsg ->
            let
                ( blogModels, cmd ) =
                    Blogs.Types.update blogMsg model.blogModels
            in
            ( { model | blogModels = blogModels }
            , Cmd.map BlogMsg cmd
            )

        Nop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Element.layout
            []
            (column
                [ width <| Element.maximum 1080 fill
                , centerX
                ]
                [ banner
                , case model.route of
                    Root ->
                        el
                            [ onLeft sidebar ]
                            blogPreviews

                    Blog title ->
                        let
                            toShow =
                                case Blogs.Index.findByTitle model.blogModels title of
                                    Nothing ->
                                        text "Error: The URL doesn't match any blog."

                                    Just blog ->
                                        blog.view
                        in
                        Element.map BlogMsg toShow

                    NotFound ->
                        paragraph
                            [ padding 20 ]
                            [ text "Error: The URL doesn't match any route." ]
                ]
            )
        ]
    }


banner : Element msg
banner =
    el
        [ width fill
        , Background.color black
        ]
        (column
            [ paddingXY 40 32
            , spacing 20
            , Font.size 32
            , Font.color white
            ]
            [ text "Technical"
            , text "Mindfulness"
            ]
        )


blogPreviews : Element msg
blogPreviews =
    column
        [ padding 20 ]
        [ text "blogs" ]


sidebar : Element msg
sidebar =
    el
        [ paddingXY 40 80 ]
        (column
            [ width <| Element.px 160
            ]
            [ plainPara "Filter blogs with one or more tags."

            -- Sidebar buttons
            ]
        )


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
