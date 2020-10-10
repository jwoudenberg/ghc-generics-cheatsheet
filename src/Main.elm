module Main exposing (main)

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
import Url.Parser


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
        exampleParser example =
            Url.Parser.map (ExamplePage example) (Url.Parser.s example.path)

        parser =
            Url.Parser.oneOf <|
                Url.Parser.map IndexPage Url.Parser.top
                    :: List.map exampleParser examples
    in
    Url.Parser.parse parser url
        |> Maybe.withDefault IndexPage


urlForPage : Page -> String
urlForPage page =
    case page of
        IndexPage ->
            "/"

        ExamplePage example ->
            "/" ++ Url.Builder.relative [ example.path ] []


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
                    (List.map viewSummary examples)
                ]

        ExamplePage example ->
            viewExample example


viewSummary : Example -> Html Msg
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
            [ example.originalType |> format example.formatting
            ]
        ]


viewExample : Example -> Html Msg
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
            , Html.td [ Attr.css [ cellStyles ] ] [ example.originalType |> format example.formatting ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.genericsType |> format example.formatting ]
            ]
        , Html.tr []
            [ Html.th [ Attr.css [ headerStyles ] ] [ Html.text "Example Value" ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.originalValue |> format example.formatting ]
            , Html.td [ Attr.css [ cellStyles ] ] [ example.genericsValue |> format example.formatting ]
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
    | ExamplePage Example


type Msg
    = NavigateTo Browser.UrlRequest
    | NavigatedTo Url


type alias Example =
    { path : String
    , originalType : String
    , originalValue : String
    , genericsType : String
    , genericsValue : String
    , formatting : List ( String, String -> Html Msg )
    }


examples : List Example
examples =
    [ { path = "id"
      , originalType = "newtype Id = MkId Int"
      , originalValue = "MkId 5"
      , genericsType =
            """
            type instance Rep Id
              = D1
                  ('MetaData "Id" "Ghci2" "interactive" 'True)
                  (C1
                    ('MetaCons "MkId" 'PrefixI 'False)
                    (S1
                        ('MetaSel
                          'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                        (Rec0 Int)))
            """
      , genericsValue = "M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 5}}}}"
      , formatting =
            [ ( "MkId", blue )
            , ( "Id", orange )
            , ( "Int", red )
            ]
      }
    , { path = "weather"
      , originalType = "data Weather = Sunny | Cloudy | Rainy"
      , originalValue = "Cloudy"
      , genericsType =
            """
            type instance Rep Weather
              = D1
                  ('MetaData "Weather" "Ghci3" "interactive" 'False)
                  (C1 ('MetaCons "Sunny" 'PrefixI 'False) U1
                  :+: (C1 ('MetaCons "Cloudy" 'PrefixI 'False) U1
                        :+: C1 ('MetaCons "Rainy" 'PrefixI 'False) U1))
            """
      , genericsValue = "M1 {unM1 = R1 (L1 (M1 {unM1 = U1}))}"
      , formatting = []
      }
    , { path = "boardgame"
      , originalType =
            """
            data BoardGame =
              BoardGame
                { name :: Text
                , maxPlayers :: Int
                , genre :: Genre
                }
            """
      , originalValue =
            """
            BoardGame
              { name = "Inis"
              , maxPlayers = 4
              , genre = Strategy
              }
            """
      , genericsType =
            """
            type instance Rep BoardGame
              = D1
                  ('MetaData "BoardGame" "Ghci8" "interactive" 'False)
                  (C1
                    ('MetaCons "BoardGame" 'PrefixI 'True)
                    (S1
                        ('MetaSel ('Just "name") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                        (Rec0 Text)
                      :*: (S1
                            ('MetaSel ('Just "maxPlayers") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                            (Rec0 Int)
                          :*: S1
                                ('MetaSel ('Just "genre") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                                (Rec0 Genre))))
            """
      , genericsValue =
            """
            M1 (
              M1 (
                M1 (K1 "Inis") :*: ( M1 (K1 4) :*: M1 (K1 Strategy) )
              )
            )
            """
      , formatting = []
      }
    , { path = "unicorn"
      , originalType = "data Unicorn"
      , originalValue = "n/a"
      , genericsType =
            """
            type instance Rep Unicorn
              = D1 ('MetaData "Unicorn" "Ghci17" "interactive" 'False) V1
            """
      , genericsValue = "n/a"
      , formatting = []
      }
    ]


orange : String -> Html msg
orange string =
    Html.span
        [ Attr.css
            [ Css.color (Css.hex "#ffa500")
            , hoverStyles
            ]
        ]
        [ Html.text string ]


blue : String -> Html msg
blue string =
    Html.span
        [ Attr.css
            [ Css.color (Css.hex "#2b74c1")
            , hoverStyles
            ]
        ]
        [ Html.text string ]


red : String -> Html msg
red string =
    Html.span
        [ Attr.css
            [ Css.color (Css.hex "#ff0000")
            , hoverStyles
            ]
        ]
        [ Html.text string ]


format : List ( String, String -> Html msg ) -> String -> Html msg
format replacements_ string =
    let
        replacements =
            replacements_
                |> List.map (\( key, modifier ) -> ( key, modifier key ))
    in
    String.Extra.unindent string
        |> String.trim
        |> replaceWithHtml replacements
        |> Html.code [ Attr.css [ Css.whiteSpace Css.pre ] ]


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
