module Authored exposing (Annotation, Example, examples)


type alias Example =
    { path : String
    , originalType : String
    , originalValue : String
    , genericsType : String
    , genericsValue : String
    , annotations : List Annotation
    }


type alias Annotation =
    { keyword : String
    , annotation : String
    }


examples : List Example
examples =
    [ { path = "id"
      , originalType = "newtype Id = MkId Int"
      , originalValue = "MkId 5"
      , genericsType =
            """
            type instance Rep Id
              = M1 D
                   ('MetaData "Id" "My.Module" "my-package" 'True)
                   (M1 C
                       ('MetaCons "MkId" 'PrefixI 'False)
                       (M1 S
                           ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                           (Rec0 Int)
                       )
                    )
            """
      , genericsValue = "M1 (M1 (M1 (K1 5)))"
      , annotations =
            [ m1Annotation
            , { keyword = "MkId"
              , annotation =
                    """
                    # Constructor name

                    The constructor name is provided as metadata in the generics representation.

                    ```hs
                    newtype Id = MkId Int
                    ```
                    """
              }
            , typeNameAnnotation "Id"
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
      , genericsValue = "M1 (R1 (L1 (M1 U1)))"
      , annotations =
            [ m1Annotation
            , typeNameAnnotation "Weather"
            ]
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
      , annotations =
            [ m1Annotation
            , typeNameAnnotation "BoardGame"
            ]
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
      , annotations =
            [ m1Annotation
            , typeNameAnnotation "Unicorn"
            ]
      }
    ]


m1Annotation : Annotation
m1Annotation =
    { keyword = "M1"
    , annotation =
        """
        # M1 contains metadata

        The `M1` type attaches metadata to the type it wraps.
        We see a lot of these in Generics representations,
        because every type, type constructor, and type parameter is wrapped in one.
        
        The definition of `M1` is:

        ```haskell
        newtype M1 kind meta f p = M1 (f p)
        ```

        Depending on what is being wrapped we'll encounter one of three pairs of `kind` and `meta` parameters.

        | wrapped type  | `kind` | `meta`                                        |
        | ------------- | ------ | --------------------------------------------- |
        | a type        | `D`    | `'MetaData typename module package isnewtype` |
        | a constructor | `C`    | `'MetaCons constructorname fixity isrecord`   |
        | a parameter   | `S`    | `'MetaSel fieldname unpacked strict strict2`  |

        As you can see there's a lot of information attached here.
        To learn more about what it all means open the details pages for one of the metadata types.

        Beware that `M1` sometimes goes by the name `D1`, `C1`, or `S1`.
        These are type synonyms for `M1` with the `kind` parameter pre-applied:

        ```haskell
        type D1 = M1 D
        type C1 = M1 C
        type S1 = M1 S
        ```
        """
    }


typeNameAnnotation : String -> Annotation
typeNameAnnotation keyword =
    { keyword = keyword
    , annotation =
        """
        # Type Name
        
        The name of the type is available as a type-level string on the metadata `M1` wrapping the type.

        We can get type name as a regular string using the [`symbolVal`][symbolval] function in GHC's standard library.
        In the example below we use it to define a function that can get the name of any type with a `Generic` instance.

        ```haskell
        {-# LANGUAGE DataKinds #-}
        {-# LANGUAGE DeriveGeneric #-}
        {-# LANGUAGE FlexibleContexts #-}
        {-# LANGUAGE FlexibleInstances #-}
        {-# LANGUAGE ScopedTypeVariables #-}

        module Example (main) where

        import Data.Proxy (Proxy (Proxy))
        import GHC.Generics (D, Generic, M1, Meta (MetaData), Rep, from)
        import GHC.TypeLits (KnownSymbol, symbolVal)

        -- | Return the name of any type with a `Generic` instance.
        typeName :: (Generic a, GetTypeName (Rep a)) => a -> String
        typeName val = getTypeName (from val)

        class GetTypeName f where
          getTypeName :: f p -> String

        instance (KnownSymbol typename) =>
                GetTypeName (M1 D ('MetaData typename m p n) f) where
          getTypeName _ = symbolVal (Proxy :: Proxy typename)

        -- Okay, let's try this out!

        data ExampleType = ExampleValue deriving (Generic)

        main :: IO ()
        main = putStrLn (typeName ExampleValue) -- -> "ExampleType"
        ```

        You can run this example yourself by copying it into a file called `Example.hs`, then running `runghc Example.hs`.

        [symbolval]: https://www.stackage.org/haddock/lts-16.17/base-4.13.0.0/GHC-TypeLits.html#v:symbolVal
        """
    }
