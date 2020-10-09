module Main exposing (main)

import Browser
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import List
import String.Extra


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view = \_ -> Html.toUnstyled <| Html.div [] (List.map view examples)
        }


view : Example msg -> Html msg
view example =
    Html.section []
        [ Html.h2 [] [ Html.text example.originalType ]
        , Html.table []
            [ Html.tr []
                [ Html.th [] []
                , Html.th [] [ Html.text "Original" ]
                , Html.th [] [ Html.text "Generic representation" ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Type" ]
                , Html.td [] [ example.originalType |> format example.formatting ]
                , Html.td [] [ example.genericsType |> format example.formatting ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Example Value" ]
                , Html.td [] [ example.originalValue |> format example.formatting ]
                , Html.td [] [ example.genericsValue |> format example.formatting ]
                ]
            ]
        ]


type alias Example msg =
    { originalType : String
    , originalValue : String
    , genericsType : String
    , genericsValue : String
    , formatting : List ( String, String -> Html msg )
    }


examples : List (Example msg)
examples =
    [ { originalType = "newtype Id = MkId Int"
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
            [ ( "Id", orange )
            , ( "Int", red )
            ]
      }
    , { originalType = "data Weather = Sunny | Cloudy | Rainy"
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
    , { originalType =
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
                        ('MetaSel
                          ('Just "name")
                          'NoSourceUnpackedness
                          'NoSourceStrictness
                          'DecidedLazy)
                        (Rec0 Text)
                      :*: (S1
                            ('MetaSel
                                ('Just "maxPlayers")
                                'NoSourceUnpackedness
                                'NoSourceStrictness
                                'DecidedLazy)
                            (Rec0 Int)
                          :*: S1
                                ('MetaSel
                                    ('Just "genre")
                                    'NoSourceUnpackedness
                                    'NoSourceStrictness
                                    'DecidedLazy)
                                (Rec0 Genre))))
            """
      , genericsValue = "M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = \"Inis\"}} :*: (M1 {unM1 = K1 {unK1 = 4}} :*: M1 {unM1 = K1 {unK1 = Strategy}})}}"
      , formatting = []
      }
    , { originalType = "data Unicorn"
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
            ]
        ]
        [ Html.text string ]


red : String -> Html msg
red string =
    Html.span
        [ Attr.css
            [ Css.color (Css.hex "#ff0000")
            ]
        ]
        [ Html.text string ]


format : List ( String, String -> Html msg ) -> String -> Html msg
format replacements_ string =
    let
        replacements =
            replacements_
                |> List.map (\( key, modifier ) -> ( key, modifier key ))
                |> Dict.fromList
    in
    breakUpAround (Dict.keys replacements) [ String.Extra.unindent string ]
        |> List.map
            (\fragment ->
                Dict.get fragment replacements
                    |> Maybe.withDefault (Html.text fragment)
            )
        |> Html.code [ Attr.css [ Css.whiteSpace Css.pre ] ]


breakUpAround : List String -> List String -> List String
breakUpAround breakers fragments =
    case breakers of
        [] ->
            fragments

        breaker :: rest ->
            fragments
                |> List.concatMap
                    (\fragment ->
                        String.split breaker fragment
                            |> List.intersperse breaker
                            |> List.filter (not << String.isEmpty)
                    )
                |> breakUpAround rest
