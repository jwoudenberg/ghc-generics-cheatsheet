module Main exposing (main)

import Authored
import Browser
import Browser.Navigation exposing (Key)
import Colors
import Css
import Css.Global
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import List
import Markdown
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


parseUrl : Url -> Page (Html Msg)
parseUrl url =
    let
        parser =
            Url.Parser.oneOf <|
                Url.Parser.map IndexPage Url.Parser.top
                    :: List.map exampleParser htmlExamples

        exampleParser example =
            Url.Parser.s example.path
                </> (Url.Parser.oneOf <|
                        Url.Parser.map (ExamplePage example) Url.Parser.top
                            :: List.map (annotationParser example) example.annotations
                    )

        annotationParser example annotation =
            Url.Parser.map
                (AnnotationPage example annotation)
                (Url.Parser.s (toParam annotation.keyword))
    in
    Url.Parser.parse parser url
        |> Maybe.withDefault IndexPage


urlForPage : Page a -> String
urlForPage page =
    let
        segments =
            case page of
                IndexPage ->
                    []

                ExamplePage example ->
                    [ example.path ]

                AnnotationPage example annotation ->
                    [ example.path, toParam annotation.keyword ]
    in
    "/" ++ Url.Builder.relative segments []


view : Page (Html Msg) -> Html Msg
view page =
    Html.div
        [ Attr.css
            [ Css.position Css.absolute
            , Css.minWidth (Css.vw 100)
            , Css.minHeight (Css.vh 100)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            ]
        ]
        [ Css.Global.global
            [ Css.Global.typeSelector "body"
                [ Css.backgroundColor (Css.hex "#ee5185")
                , Css.fontFamilies [ "Helvetica Neue", "Helvetica", "Arial", "sans-serif" ]
                ]
            ]
        , viewPage page
        , Html.footer
            [ Attr.css
                [ Css.textAlign Css.center
                , Css.opacity (Css.num 0.4)
                , Css.padding (Css.px 10)
                ]
            ]
            [ Html.text "© 2020 Jasper Woudenberg - "
            , Html.a
                [ Attr.css
                    [ Css.color Css.inherit
                    ]
                , Attr.href "https://github.com/jwoudenberg/ghc-generics-cheatsheet"
                ]
                [ Html.text "This Code"
                ]
            , Html.text " - "
            , Html.a
                [ Attr.css
                    [ Css.color Css.inherit
                    ]
                , Attr.href "https://dev.to/jwoudenberg"
                ]
                [ Html.text "My Blog"
                ]
            ]
        ]


viewPage : Page (Html Msg) -> Html Msg
viewPage page =
    case page of
        IndexPage ->
            Html.div
                [ Attr.css
                    [ Css.margin2 Css.zero Css.auto
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
                    (List.map viewSummary htmlExamples)
                ]

        ExamplePage example ->
            viewExample example

        AnnotationPage example annotation ->
            let
                options : Markdown.Options
                options =
                    { githubFlavored = Just { tables = True, breaks = False }
                    , defaultHighlighting = Nothing
                    , sanitize = True
                    , smartypants = True
                    }

                markdown =
                    annotation.annotation
                        |> String.Extra.unindent
                        |> String.trim
                        |> Markdown.toHtmlWith options []
                        |> Html.fromUnstyled
            in
            Html.div []
                [ viewBackArrow (ExamplePage example)
                , Html.div
                    [ Attr.css
                        [ Css.width (Css.px 700)
                        , Css.backgroundColor (Css.hex "#fff")
                        , Css.padding (Css.px 20)
                        , Css.boxShadow4 (Css.px 1) (Css.px 1) (Css.px 2) (Css.hex "#333")
                        , Css.borderRadius (Css.px 2)
                        , Css.margin2 (Css.px 10) Css.auto
                        , Css.Global.descendants
                            [ Css.Global.typeSelector "h1"
                                [ Css.marginTop (Css.px 10)
                                , Css.color (Css.hex "#333")
                                ]
                            , Css.Global.typeSelector "p"
                                [ Css.fontSize (Css.em 1.1)
                                ]
                            , Css.Global.typeSelector "a"
                                [ Css.color Css.inherit
                                , Css.color (Css.hex "#ee5185")
                                , Css.textDecoration Css.none
                                ]
                            , Css.Global.typeSelector "code"
                                [ Css.fontSize (Css.em 1.1)
                                ]
                            , Css.Global.typeSelector "pre"
                                [ Css.backgroundColor (Css.hex "#333")
                                , Css.color (Css.hex "#fff")
                                , Css.lineHeight (Css.em 1.4)
                                , Css.display Css.block
                                , Css.padding2 (Css.px 20) (Css.px 30)
                                , Css.margin2 Css.zero (Css.px -20)
                                ]
                            , Css.Global.typeSelector "table"
                                [ Css.margin (Css.px 10)
                                ]
                            , Css.Global.typeSelector "td"
                                [ Css.paddingRight (Css.em 2)
                                ]
                            , Css.Global.typeSelector "th"
                                [ Css.paddingRight (Css.em 2)
                                , Css.textAlign Css.left
                                ]
                            ]
                        ]
                    ]
                    [ markdown
                    ]
                ]


viewSummary : Authored.Example (Html Msg) -> Html Msg
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
            [ example.originalType
            ]
        ]


viewExample : Authored.Example (Html Msg) -> Html Msg
viewExample example =
    Html.table
        [ Attr.css
            [ Css.minWidth (Css.vw 100)
            , Css.borderCollapse Css.separate
            , Css.borderSpacing (Css.px 20)
            ]
        ]
        [ viewBackArrow IndexPage
        , Html.tr []
            [ Html.th [] []
            , Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Original" ]
            , Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Generic representation" ]
            ]
        , Html.tr []
            [ Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Type" ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.originalType ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.genericsType ]
            ]
        , Html.tr []
            [ Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Example Value" ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.originalValue ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.genericsValue ]
            ]
        ]


viewBackArrow : Page a -> Html Msg
viewBackArrow page =
    Html.a
        [ Attr.href (urlForPage page)
        , Attr.css
            [ headerStyles
            , Css.position Css.absolute
            , Css.left Css.zero
            , Css.top Css.zero
            , Css.lineHeight Css.zero
            , Css.padding (Css.px 30)
            , Css.fontSize (Css.em 4)
            , Css.textDecoration Css.none
            , Css.display Css.block
            ]
        ]
        [ Html.text "‹"
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


type alias Model =
    { page : Page (Html Msg)
    , key : Key
    }


type Page a
    = IndexPage
    | ExamplePage (Authored.Example a)
    | AnnotationPage (Authored.Example a) Authored.Annotation


type Msg
    = NavigateTo Browser.UrlRequest
    | NavigatedTo Url


htmlExamples : List (Authored.Example (Html Msg))
htmlExamples =
    List.map htmlifyExample Authored.examples


htmlifyExample : Authored.Example String -> Authored.Example (Html Msg)
htmlifyExample example =
    { path = example.path
    , originalType = viewExampleText .originalType example
    , originalValue = viewExampleText .originalValue example
    , genericsType = viewExampleText .genericsType example
    , genericsValue = viewExampleText .genericsValue example
    , annotations = example.annotations
    }


viewExampleText : (Authored.Example String -> String) -> Authored.Example String -> Html msg
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
            Html.span
                [ Attr.css [ Css.display Css.inlineBlock ]
                ]
                [ Html.a
                    [ Attr.href (urlForPage (AnnotationPage example annotation))
                    , Attr.css
                        [ Css.color Css.inherit
                        , Css.textDecoration Css.none
                        , color
                        , Css.cursor Css.pointer
                        , Css.borderRadius (Css.px 4)
                        , Css.display Css.block
                        , Css.padding (Css.px 15)
                        , Css.margin (Css.px -15)
                        , Css.hover
                            [ Css.textDecoration Css.underline
                            ]
                        ]
                    ]
                    [ Html.text annotation.keyword ]
                ]
    in
    String.Extra.unindent (getString example)
        |> String.trim
        |> replaceWithHtml replacements
        |> Html.code [ Attr.css [ Css.whiteSpace Css.pre ] ]


colorscheme : List Css.Style
colorscheme =
    List.map
        (\color -> Css.batch [ Css.color (Css.hex color) ])
        Colors.colors


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


toParam : String -> String
toParam str =
    String.toLower str
        |> String.replace " " "-"
        |> String.replace "'" ""
