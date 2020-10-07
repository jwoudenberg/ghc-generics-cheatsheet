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
        , view = \_ -> Html.toUnstyled view
        }


view : Html msg
view =
    Html.table []
        [ Html.tr []
            [ Html.th [] []
            , Html.th [] [ Html.text "Type" ]
            , Html.th [] [ Html.text "Example Value" ]
            ]
        , Html.tr []
            [ Html.th [] [ Html.text "Original" ]
            , Html.td [] [ Html.code [] [ Html.text "newtype Id = Id Int" ] ]
            , Html.td [] [ Html.code [] [ Html.text "Id 5" ] ]
            ]
        , Html.tr []
            [ Html.th [] [ Html.text "Generic Representation" ]
            , Html.td []
                [ Html.code
                    [ Attr.css [ Css.whiteSpace Css.pre ]
                    ]
                    [ """
                      type instance Rep Id
                        = D1
                            ('MetaData "Id" "Ghci1" "interactive" 'True)
                            (C1
                              ('MetaCons "Id" 'PrefixI 'False)
                              (S1
                                  ('MetaSel
                                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                                  (Rec0 Int)))
                      """
                        |> format
                            [ ( "Id", orange )
                            , ( "Int", red )
                            ]
                    ]
                ]
            , Html.td [] [ Html.code [] [ Html.text "Id 5" ] ]
            ]
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
