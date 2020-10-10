module Main exposing (main)

import Authored
import Browser
import Browser.Navigation exposing (Key)
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import List
import String.Extra
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>))


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url key ->
                update (NavigatedTo url)
                    { page = IndexPage
                    , key = key
                    }
        , update = update
        , view =
            \model ->
                { title = "Generics Cheatsheet"
                , body = [ Html.toUnstyled (view model.page) ]
                }
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = NavigateTo
        , onUrlChange = NavigatedTo
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateTo (Browser.External url) ->
            ( model
            , Browser.Navigation.load url
            )

        NavigateTo (Browser.Internal url) ->
            ( model
            , let
                normalizedUrl =
                    urlForPage (parseUrl url)
              in
              if normalizedUrl == urlForPage model.page then
                Cmd.none

              else
                Browser.Navigation.pushUrl model.key normalizedUrl
            )

        NavigatedTo url ->
            let
                newPage =
                    parseUrl url

                normalizedUrl =
                    urlForPage newPage
            in
            ( if normalizedUrl == urlForPage model.page then
                model

              else
                { model | page = newPage }
            , if url.path == normalizedUrl then
                Cmd.none

              else
                Browser.Navigation.replaceUrl model.key normalizedUrl
            )


parseUrl : Url -> Page
parseUrl url =
    let
        parser =
            Url.Parser.oneOf <|
                Url.Parser.map IndexPage Url.Parser.top
                    :: List.map exampleParser Authored.examples

        exampleParser example =
            Url.Parser.s example.path
                </> (Url.Parser.oneOf <|
                        Url.Parser.map (ExamplePage example) Url.Parser.top
                            :: List.map (annotationParser example) example.annotations
                    )

        annotationParser example annotation =
            Url.Parser.map
                (AnnotationPage example annotation)
                (Url.Parser.s (String.toLower annotation.keyword))
    in
    Url.Parser.parse parser url
        |> Maybe.withDefault IndexPage


urlForPage : Page -> String
urlForPage page =
    let
        segments =
            case page of
                IndexPage ->
                    []

                ExamplePage example ->
                    [ example.path ]

                AnnotationPage example annotation ->
                    [ example.path, String.toLower annotation.keyword ]
    in
    "/" ++ Url.Builder.relative segments []


view : Page -> Html Msg
view page =
    Html.div
        [ Attr.css
            [ Css.backgroundColor (Css.hex "#ee5185")
            , Css.position Css.absolute
            , Css.minWidth (Css.vw 100)
            , Css.minHeight (Css.vh 100)
            , Css.fontFamilies [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ]
            ]
        ]
        [ viewPage page ]


viewPage : Page -> Html Msg
viewPage page =
    case page of
        IndexPage ->
            Html.div
                [ Attr.css
                    [ Css.margin Css.auto
                    , Css.width (Css.px 700)
                    ]
                ]
                [ Html.h1
                    [ Attr.css
                        [ Css.marginTop (Css.em 1.2)
                        , Css.marginBottom (Css.em 0.8)
                        , Css.textAlign Css.center
                        , headerStyles
                        ]
                    ]
                    [ Html.text "GHC Generics Cheat Sheet" ]
                , Html.ul
                    [ Attr.css
                        [ Css.listStyle Css.none
                        , Css.margin Css.zero
                        , Css.padding Css.zero
                        ]
                    ]
                    (List.map viewSummary Authored.examples)
                ]

        ExamplePage example ->
            viewExample example

        AnnotationPage example annotation ->
            Html.text annotation.annotation


viewSummary : Authored.Example -> Html Msg
viewSummary example =
    Html.li
        [ Attr.css
            [ Css.marginBottom (Css.px 20)
            , Css.fontSize (Css.em 2)
            , Css.lineHeight (Css.em 1)
            , Css.borderRadius (Css.px 2)
            , Css.boxShadow4 (Css.px 1) (Css.px 1) (Css.px 2) (Css.hex "#333")
            , Css.backgroundColor (Css.hex "#fff")
            ]
        ]
        [ Html.a
            [ Attr.href (urlForPage (ExamplePage example))
            , Attr.css
                [ Css.color Css.inherit
                , Css.textDecoration Css.none
                , Css.display Css.block
                , Css.padding (Css.px 10)
                ]
            ]
            [ viewExampleText .originalType example
            ]
        ]


viewExample : Authored.Example -> Html Msg
viewExample example =
    Html.table
        [ Attr.css
            [ Css.minWidth (Css.vw 100)
            , Css.borderCollapse Css.separate
            , Css.borderSpacing (Css.px 20)
            ]
        ]
        [ Html.a
            [ Attr.href (urlForPage IndexPage)
            , Attr.css
                [ headerStyles
                , Css.position Css.absolute
                , Css.left (Css.px 30)
                , Css.top Css.zero
                , Css.fontSize (Css.em 4)
                , Css.textDecoration Css.none
                , Css.display Css.block
                ]
            ]
            [ Html.text "‹"
            ]
        , Html.tr []
            [ Html.th [] []
            , Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Original" ]
            , Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Generic representation" ]
            ]
        , Html.tr []
            [ Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Type" ]
            , Html.td [ Attr.css [ cellStyles ] ] [ viewExampleText .originalType example ]
            , Html.td [ Attr.css [ cellStyles ] ] [ viewExampleText .genericsType example ]
            ]
        , Html.tr []
            [ Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Example Value" ]
            , Html.td [ Attr.css [ cellStyles ] ] [ viewExampleText .originalValue example ]
            , Html.td [ Attr.css [ cellStyles ] ] [ viewExampleText .genericsValue example ]
            ]
        ]


cellStyles : Css.Style
cellStyles =
    Css.batch
        [ Css.backgroundColor (Css.hex "#fefefe")
        , Css.borderRadius (Css.px 2)
        , Css.padding (Css.px 20)
        , Css.fontSize (Css.em 1.1)
        , Css.lineHeight (Css.em 0.9)
        , Css.boxShadow4 (Css.px 1) (Css.px 1) (Css.px 2) (Css.hex "#333")
        ]


headerStyles : Css.Style
headerStyles =
    Css.batch
        [ Css.color (Css.hex "#fff")
        , Css.textShadow4 (Css.px 1) (Css.px 1) (Css.px 2) (Css.hex "#000")
        ]


hoverStyles : Css.Style
hoverStyles =
    Css.batch
        [ Css.cursor Css.pointer
        , Css.borderRadius (Css.px 4)
        , Css.hover
            [ Css.backgroundColor (Css.hex "#eee")
            , Css.boxShadow4 (Css.px 0) (Css.px 0) (Css.px 4) (Css.hex "#eee")
            ]
        ]


type alias Model =
    { page : Page
    , key : Key
    }


type Page
    = IndexPage
    | ExamplePage Authored.Example
    | AnnotationPage Authored.Example Authored.Annotation


type Msg
    = NavigateTo Browser.UrlRequest
    | NavigatedTo Url


viewExampleText : (Authored.Example -> String) -> Authored.Example -> Html msg
viewExampleText getString example =
    let
        colors =
            colorscheme
                ++ List.repeat (List.length example.annotations) (Css.batch [])

        replacements =
            List.map2
                (\annotation color -> ( annotation.keyword, formatKeyword annotation color ))
                example.annotations
                colors

        formatKeyword : Authored.Annotation -> Css.Style -> Html msg
        formatKeyword annotation color =
            Html.a
                [ Attr.href (urlForPage (AnnotationPage example annotation))
                , Attr.css
                    [ Css.color Css.inherit
                    , Css.textDecoration Css.none
                    , color
                    , hoverStyles
                    ]
                ]
                [ Html.text annotation.keyword ]
    in
    String.Extra.unindent (getString example)
        |> String.trim
        |> replaceWithHtml replacements
        |> Html.code [ Attr.css [ Css.whiteSpace Css.pre ] ]


colorscheme : List Css.Style
colorscheme =
    List.map (\color -> Css.batch [ Css.color (Css.hex color) ])
        [ "#ff0000"
        , "#2b74c1"
        , "#ffa500"
        ]


replaceWithHtml : List ( String, Html msg ) -> String -> List (Html msg)
replaceWithHtml breakers string =
    case breakers of
        [] ->
            if String.isEmpty string then
                []

            else
                [ Html.text string ]

        ( breaker, replacement ) :: rest ->
            String.split breaker string
                |> List.map (replaceWithHtml rest)
                |> List.intersperse [ replacement ]
                |> List.concatMap identity
