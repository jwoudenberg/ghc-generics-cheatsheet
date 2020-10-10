module Authored exposing (Example, examples)


type alias Example =
    { path : String
    , originalType : String
    , originalValue : String
    , genericsType : String
    , genericsValue : String
    , annotations : List ( String, String )
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
      , annotations =
            [ ( "MkId", "Constructor" )
            , ( "Id", "Type name" )
            , ( "Int", "Wrapped type" )
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
      , annotations = []
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
      , annotations = []
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
      , annotations = []
      }
    ]
