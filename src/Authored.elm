module Authored exposing (Annotation, Example, examples)


type alias Example a =
    { path : String
    , originalType : a
    , originalValue : a
    , genericsType : a
    , genericsValue : a
    , annotations : List Annotation
    }


type alias Annotation =
    { path : String
    , keyword : String
    , annotation : String
    }


examples : List (Example String)
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
                           (K1 R Int)
                       )
                    )
            """
      , genericsValue = "M1 (M1 (M1 (K1 5)))"
      , annotations =
            [ m1Annotation
            , k1Annotation
            , metadataAnnotation
            , metaconsAnnotation
            , metaselAnnotation
            , constructorNameAnnotation "MkId"
            , typeNameAnnotation "Id"
            , wrappedTypeAnnotation "Int"
            , wrappedValueAnnotation "5"
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
            , constructorNameAnnotation "Sunny"
            , constructorNameAnnotation "Cloudy"
            , constructorNameAnnotation "Rainy"
            , typeinstanceAnnotation
            ]
                ++ sumAnnotations
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
                           (K1 R Text)
                        :*:
                        (M1 S
                            ('MetaSel ('Just "maxPlayers") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                            (K1 R Int)
                         :*:
                         M1 S
                            ('MetaSel ('Just "genre") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                            (K1 R Genre))))
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
            , k1Annotation
            , metadataAnnotation
            , metaconsAnnotation
            , metaselAnnotation
            , productAnnotation
            , typeNameAnnotation "BoardGame"
            , constructorNameAnnotation "Stats"
            , fieldNameAnnotation "name"
            , fieldNameAnnotation "maxPlayers"
            , fieldNameAnnotation "genre"
            , wrappedTypeAnnotation "Text"
            , wrappedTypeAnnotation "Int"
            , wrappedTypeAnnotation "Genre"
            , wrappedValueAnnotation "Inis"
            , wrappedValueAnnotation "4"
            , wrappedValueAnnotation "Strategy"
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
    { path = "m1"
    , keyword = "M1"
    , annotation =
        """
        # `M1` contains metadata

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


k1Annotation : Annotation
k1Annotation =
    { path = "k1"
    , keyword = "K1"
    , annotation =
        """
        # `K1` wraps included types

        Types we write often contain other types. For example, the `Treasure`
        type below contains the `Coords` and `Int` types.

        ```haskell
        data Treasure = Treasure { where :: Coords, guilders :: Int }
                                            ^^^^^^              ^^^
        ```

        When we add `deriving (Generic)` to a type GHC produces a generic
        representation of the type but doesn't recurse into its component
        types. Those compont types are still around in the generic
        representation, wrapped in a `K1`.

        `K1` is defined as follows:

        ```haskell
        newtype K1 rec c p = K1 c 
        ```

        There's two phantom parameters `rec` and `p`, and neither has any
        meaning. `p` exists solely for future exensibility and doesn't have a
        purpose today (all generics building blocks have this phantom type `p`
        and pass it through). `rec` used to have two possible values, but one
        got deprecated, so now it's always the same thing: `R`.

        When printing generics representations of types `K1` often shows up
        under a different name, `Rec0`. That's `K1` with `R` already applied:

        ```haskell
        type Rec0 = K1 R
        ```
        """
    }


sumAnnotations : List Annotation
sumAnnotations =
    [ ":+:", "L1", "R1" ]
        |> List.map
            (\keyword ->
                { path = "sum"
                , keyword = keyword
                , annotation =
                    """
                    # `:+:` is `Either` by another name (almost)

                    `:+:` combines types in the same way that `Either` does.
                    The only difference is `:+:` carries around an extra `p` parameter.

                    ```haskell
                    data Either a b   = Left a    | Right b
                    data (:+:)  a b p = L1  (a p) | R1   (b p)
                    ```

                    `:+:` is used to represent a type that has multiple
                    constructors. Such a type is sometimes refered to an _ADT_
                    or _sum type_ (that's why there's a plus in the type). In
                    the generics representation of a type with two constructors
                    each constructor will end up on one side of the `:+:`.

                    If a type has more than two constructors it will take
                    multiple `:+:` two combine them all together. Haskell will
                    build a tree using `:+:` to contain all of them.

                    ```haskell
                    data DoSpell = Chant Text | Smash Relic | Drink Potion
                    
                    -- Leaving out wrappers like M1 and K1 the generics
                    -- representation of `Color` will look something like this:
                    -- 
                    --     Text :+: (Relic :+: Potion)
                    ```

                    This is similar to how we might define `DoSpell` as a type
                    synonym in a hypothetical language like Haskell, but without
                    support for custom types:

                    ```haskell
                    type DoSpellWorse = Either Text (Either Relic Potion)
                    ```

                    Values of `:+:` are created using `L1` and `R1`
                    constructors, which function the same as the `Left` and
                    `Right` constructors of the `Either` type. Below are three
                    examples, each using a different constructor of the
                    `DoSpell` example type above.

                    | `DoSpell` value    | `Rep DoSpell` value  | `DoSpellWorse` value       |
                    | ------------------ | -------------------- | -------------------------- |
                    | `Chant "AWAKEN!"`  | `L1 "AWAKEN!"`       | `Left "AWAKEN!"`           |
                    | `Smash Medallion`  | `R1 (L1 Medallion)`  | `Right (Left Medallion)`   |
                    | `Drink Pollyjuice` | `R1 (R1 Pollyjuice)` | `Right (Right Pollyjuice)` |
                    """
                }
            )


productAnnotation : Annotation
productAnnotation =
    { path = "product"
    , keyword = ":*:"
    , annotation =
        """
        # `:*:` is a tuple (almost)

        `:*:` is used to combine two types in the same way a tuple does.
        The only difference is that `:*:` carries around an additional `p`
        parameter.

        ```hs
        data (,)   a b   = (,)    a     b
        data (:*:) a b p = (:*:) (a p) (b p)
        ```

        Haskell will use `:*:` two represent constructors with multiple
        parameters. These can be regular parameters or fields of a record.
        Types with multiple parameters are sometimes called _product types_,
        hence the multiplication symbol in `:*:`.

        ```hs
        data Ghost = Ghost { haunt :: Coords, opacity :: Double }
        data Monster = Monster Species Odor HidingPlace
        ```

        In the generics representation of a contructor with two parameters, like
        `Ghost`, those parameters land on opposite sides of a single `:*:`. If
        the constructor has three or more parameters, like `Monster`, then it
        takes multiple `:*:` to combine them all. The generics representation
        of `Monster` looks like this:

        ```
        -- pseudo code below, because wrappers like M1 and K1 are omitted!
        type instance Rep Monster = Species :*: (Odor :*: HidingPlace)
        ```

        A tuple value `(2, "Hi!")` looks a lot like its type `(Int, Text)`.
        It's the same with `:*:`. A value of the zombie type, again omitting K1
        and M1 wrappers, would look like this:
        
        ```haskell
        monster = Zombie :*: (Decay :*: UnderBed)
        ```
        """
    }


metadataAnnotation : Annotation
metadataAnnotation =
    { path = "metadata"
    , keyword = "'MetaData"
    , annotation =
        """
        # `'MetaData` describes a type

        The `'MetaData` parameters provide information about the original type.

        ```haskell
        'MetaData typename modulename packagename isnewtype
        ```

        | parameter   | meaning                                                              |
        | ----------- | -------------------------------------------------------------------- |
        | typename    | The name of the type. For `data Unicorn` this is `Unicorn`           |
        | modulename  | The module in which this type is defined, e.g. "My.Module"           |
        | packagename | The package in which this type is defined, e.g. "my-package"         |
        | isnewtype   | `'True` if defined using `newtype`, `'False` if defined using `data` |

        The prime in `'MetaData` isn't a typo and really part of the name.
        `'MetaData` gets this prime because it is defined in a peculiar way.
        We usually define types using the keywords `type`, `newtype`, and `data`, but there's another way:
        If we enable the `DataKinds` extension in a module GHC will create a type for each constructor in the module, with the same name as the constructor.
        That's why in the [documentation for `'MetaData`][metadata] we see it defined as a constructor of the `Meta` type.

        `DataKinds` will give us a type and a constructor with the same name.
        When we put a prime in front of the name we tell GHC we mean the type, not the constructor.
        In most cases GHC is able to figure out from context whether we mean the type or constructor and we can omit the prime.

        These automatically generated types `DataKinds` produces don't have any values
        so their only real use is in phantom types, which is exactly how `'MetaData'` is used!

        If you're interested in learning more about `DataKinds` check out this [blog post on kinds][kinds].

        [metadata]: https://www.stackage.org/haddock/lts-16.17/base-4.13.0.0/GHC-Generics.html#v:MetaData
        [kinds]: https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/#datatype-promotion
        """
    }


metaconsAnnotation : Annotation
metaconsAnnotation =
    { path = "metacons"
    , keyword = "'MetaCons"
    , annotation =
        """
        # `'MetaCons` describes a constructor

        `'MetaCons` contains information about a single constructor of a type.

        ```haskell
        'MetaCons constructorname fixity isrecord
        ```

        | parameter       | meaning                                                                   |
        | --------------- | ------------------------------------------------------------------------- |
        | constructorname | The name of the constructor. For `data Id = MkId` this is `MkId`          |
        | fixity          | The constructor's fixity                                                  |
        | isrecord        | `'True` if this constructor is a record: `data Rec = Rec { field : Int }` |

        The `fixity` parameter is only relevant for constructors made up entirely out of symbols.
        `NonEmpty` is one example of a type that uses such a constructor:

        ```haskell
        data NonEmpty a = a :| [a]
        ```

        The name of `NonEmpty`'s single constructor is `:|`.
        Because it's made up entirely of symbols Haskell allows us to write it in between its arguments,
        and calls such functions _infix operators_.
        The 'fixity' field provides information about how strongly the constructor binds its arguments.
        Haskell uses this information to figure out where to put the parens in expressions with multiple operators like this:

        ```haskell
        1 :| [2, 3] <> [4, 5]
        ```
        """
    }


metaselAnnotation : Annotation
metaselAnnotation =
    { path = "metasel"
    , keyword = "'MetaSel"
    , annotation =
        """
        # `'MetaSel` describes a constructor parameter

        `'MetaSel` tells us about a single parameter of a constructor.

        ```haskell
        'MetaSel fieldname unpackedness sourcestrictness decidedstrictness
        ```

        | parameter         | meaning                                                                   |
        | ----------------- | ------------------------------------------------------------------------- |
        | fieldname         | A type level `Maybe` containing the field name if this is a record field. |
        | unpackedness      | The [unpackedness][] of the field.                                        |
        | sourcestrictness  | Strictness of this parameter according to the code.                       |
        | decidedstrictness | Actual strictness decided by GHC, considering compilation flags.          |

        The `'MetaSel` is used both to describe a regular parameter and a single field of a record.
        Both are treated as parameters to a constructor.
        
        ```haskell
        data Person = Person { name : Text, age : Int }
        data Person = Person          Text        Int
        ```

        The strictness tells us whether a parameter is strict,
        meaning values stored in there will be immediately evaluated, or lazy.
        Haskell allows us to specify which we want when we define the type:

        ```haskell
        data Humidity = Humidity  Double  -- no annotation (defaults to lazy)
        data Humidity = Humidity !Double  -- explicitly strict
        data Humidity = Humidity ~Double  -- explicitly lazy
        ```

        The `sourcestrictness` option tells us which of the three options above appears in the code.
        The `decidedstrictness` option tells us what strictness GHC decided for the parameter,
        based on the code but also on compiler options, such as a the `StrictData` language extension.

        [unpackedness]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unpack-pragma
        """
    }


typeinstanceAnnotation : Annotation
typeinstanceAnnotation =
    { path = "rep"
    , keyword = "type instance Rep"
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
    { path = "typename"
    , keyword = keyword
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


fieldNameAnnotation : String -> Annotation
fieldNameAnnotation keyword =
    { metaselAnnotation | keyword = keyword }


constructorNameAnnotation : String -> Annotation
constructorNameAnnotation keyword =
    { metaconsAnnotation | keyword = keyword }


wrappedTypeAnnotation : String -> Annotation
wrappedTypeAnnotation keyword =
    { k1Annotation | keyword = keyword }


wrappedValueAnnotation : String -> Annotation
wrappedValueAnnotation keyword =
    { k1Annotation | keyword = keyword }
