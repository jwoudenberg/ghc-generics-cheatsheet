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
            , metadataAnnotation
            , metaconsAnnotation
            , metaselAnnotation
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
            , typeinstanceAnnotation
            ]
      }
    , { path = "weather"
      , originalType = "data Weather = Sunny | Cloudy | Rainy"
      , originalValue = "Cloudy"
      , genericsType =
            """
            type instance Rep Weather
              = M1 D
                   ('MetaData "Weather" "My.Module" "my-package" 'False)
                   (M1 C ('MetaCons "Sunny" 'PrefixI 'False) U1
                    :+:
                    (M1 C ('MetaCons "Cloudy" 'PrefixI 'False) U1
                     :+:
                     M1 C ('MetaCons "Rainy" 'PrefixI 'False) U1))
            """
      , genericsValue = "M1 (R1 (L1 (M1 U1)))"
      , annotations =
            [ m1Annotation
            , metadataAnnotation
            , metaconsAnnotation
            , typeNameAnnotation "Weather"
            , typeinstanceAnnotation
            ]
      }
    , { path = "boardgame"
      , originalType =
            """
            data BoardGame =
              Stats
                { name :: Text
                , maxPlayers :: Int
                , genre :: Genre
                }
            """
      , originalValue =
            """
            Stats
              { name = "Inis"
              , maxPlayers = 4
              , genre = Strategy
              }
            """
      , genericsType =
            """
            type instance Rep BoardGame
              = M1 D
                   ('MetaData "BoardGame" "My.Module" "my-package" 'False)
                   (M1 C
                       ('MetaCons "Stats" 'PrefixI 'True)
                       (M1 S
                           ('MetaSel ('Just "name") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                           (Rec0 Text)
                        :*:
                        (M1 S
                            ('MetaSel ('Just "maxPlayers") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                            (Rec0 Int)
                         :*:
                         M1 S
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
            , metadataAnnotation
            , metaconsAnnotation
            , metaselAnnotation
            , typeNameAnnotation "BoardGame"
            , typeinstanceAnnotation
            ]
      }
    , { path = "unicorn"
      , originalType = "data Unicorn"
      , originalValue = "n/a"
      , genericsType =
            """
            type instance Rep Unicorn
              = M1 D
                   ('MetaData "Unicorn" "My.Module" "my-package" 'False)
                   V1
            """
      , genericsValue = "n/a"
      , annotations =
            [ m1Annotation
            , metadataAnnotation
            , typeNameAnnotation "Unicorn"
            , typeinstanceAnnotation
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


metadataAnnotation : Annotation
metadataAnnotation =
    { keyword = "'MetaData"
    , annotation =
        """
        # 'MetaData describes a type

        The `'MetaData` parameters provide information about the original type.

        ```haskell
        MetaData typename modulename packagename isnewtype
        ```

        | parameter   | meaning                                                              |
        | ----------- | -------------------------------------------------------------------- |
        | typename    | The name of the type. For `data Foo` this is "foo"                   |
        | modulename  | The module in which this type is defined, e.g. "My.Module"           |
        | packagename | The package in which this type is defined, e.g. "my-package"         |
        | isnewtype   | `'True` if defined using `newtype`, `'False` if defined using `data` |

        The prime in `'MetaData` isn't a typo and really part of the name.
        `'MetaData` gets this prime because it is defined in a peculiar way.
        We usually define types using the keywords `type`, `newtype`, and `data`, but there's another way:
        If we enable the `DataKinds` extension in a module GHC will create a type for each constructor in the module, with the same name as the constructor.

        `DataKinds` will give us a type and a constructor with the same name.
        When we put a prime in front of the name we tell GHC we mean the type, not the constructor.
        In most uses GHC is able to figure out whether we mean the type or constructor and we can omit the prime.
        That's why in the [documentation for `'MetaData`][metadata] we see it defined as a constructor of the `Meta` type.

        These automatically generated types `DataKinds` produces don't have any values
        so their only real use is in phantom types, which is exactly how `'MetaData'` is used!

        If you're interested in learning more about `DataKinds` check out this [blog post on kinds][kinds].

        [metadata]: https://www.stackage.org/haddock/lts-16.17/base-4.13.0.0/GHC-Generics.html#v:MetaData
        [kinds]: https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/#datatype-promotion
        """
    }


metaconsAnnotation : Annotation
metaconsAnnotation =
    { keyword = "'MetaCons"
    , annotation =
        """
        """
    }


metaselAnnotation : Annotation
metaselAnnotation =
    { keyword = "'MetaSel"
    , annotation =
        """
        """
    }


typeinstanceAnnotation : Annotation
typeinstanceAnnotation =
    { keyword = "type instance Rep"
    , annotation =
        """
        # `Rep` is a type family

        GHC Generics are all about taking user defined types of which there are infinitely many,
        and converting them into a combination of a small number of primitive types.
        If that sounds a bit like a function to you then you're spot on.
        The name of the function that performs this conversion is `Rep`,
        and it takes and returns types instead of regular values.
        A function on types is sometimes called a type family.

        To understand `Rep` let's first look at a 'regular' Haskell function.
        Haskell allows us to define functions using multiple definitions, like so:

        ```haskell
        isEven :: Word -> Bool
        isEven 0  = True
        isEven n  = not (isEven (n - 1))
        ```

        We can define a type family in a similar fashion.

        ```haskell
        {-# LANGUAGE TypeFamilies #-} 

        data Yep
        data Nope

        type family HasManyValues a
        type instance HasManyValues ()        = Nope
        type instance HasManyValues Bool      = Nope
        type instance HasManyValues Int       = Yep
        type instance HasManyValues (Maybe n) = HasManyValues n
        ```

        `Rep` is very similar to `HasManyValues`.
        It also takes a single argument (an original type) and returns another type (the generic representation).
        We can manually write a line `type instance Rep MyType` if we want,
        but usually tell GHC it should do it for us by adding `deriving (Generic)` to a type.
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
