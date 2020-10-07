# Monad Transformers

We don't make a lot of use of Monad transformers in NRI Haskell code, but external libraries use them exentsively. This document is a brief introduction intended to:

- Give a high level overview of their use.
- Help recognize them in the wild.
- Provide resources for further learning.

## Description

Monad transformers aim to make working with nested monads more
pleasant. They might be easiest explained by example. Consider two functions that interact with some external calendaring service:

```hs
getBirthday :: Text -> IO (Either Error Date)
getBirthday name = ...

planParty :: Date -> Text -> IO (Either Error Party)
planParty date invite = ...
```

Suppose we want to use these to create a function that plans a birthday party. We could write the following:

```hs
planBirthdayParty :: Text -> IO (Either Error Date)
planBirthdayParty name = do
  birthdayOrError <- getBirthday name
  case birthdayOrError of
    Left err -> pure (Left err)
    Right birthday -> planParty birthday "party!"
```

The example shows that we can use the `<-` in `do` notation to peel of the `IO` of the value returned by `getBirthday`, but then we're still left with an `Either Error Date` that we have to case on. If the code would grow larger and call more `IO (Either Error Date)`-returning functions we'd need a `case ... of` statement to handle the error in each.

`Either` has a monad transformer counterpart called `ExceptT`. The transformer version of the type `IO (Either Error Date)` is `ExceptT Error IO Date`. There's a pair of functions to convert between these two representations:

```hs
ExceptT    :: IO (Either e a) -> ExceptT e IO a
runExceptT :: ExceptT e IO a  -> IO (Either e a)
```

The advantage of the `ExceptT` representation is that it's not one monad nested within another (as demonstrated by the absence of parens around the `a`). This means that in `do` notation `<-` unwraps both the `IO` and `Either` layers around the type we're interested in. We can use this to rewrite `planBirthdayParty`:

```hs
planBirthdayParty :: Text -> IO (Either Error Date)
planBirthdayParty name = runExceptT $ do
  birthday <- ExceptT $ getBirthday name
  ExceptT $ planParty birthday "party!"
```

That's pretty much it. Monad transformers temporarily smoosh monads into one to remove boiler plate in `do` blocks.

## Transformer facts

- Like `Either` and `ExceptT` other monad transformers are the counterparts of some 'regular monad'. The naming convention is to stick a `T` at the end of the name, so the `Maybe` monad has a `MonadT` transformer and the `Reader` monad a `ReaderT` transformer. `Either` and `ExceptT` are the odd ones out, not sure why.
- There's exists a pair of functions to convert between transformer and non-transformer types, like we saw exists for `ExceptT`.
- Transformers aren't limited to two layers we saw in our examples. Some applications go bigger and nest an `Either` in a `Reader` in an `IO`. Using transformers that's `ReaderT r (ExceptT e IO) a`. In `do` notation `<-` will lift out the `a` through all these layers. These kinds of constructors are sometimes referred to as 'transformer stacks'.
- There's two popular libraries providing monad transformers: [`mtl`][mtl] and[ `transformers`][transformers]. They expose some of the same stuff, for example both define an `ExceptT` type. When looking up documentation for a transformer function encountered in a library somewhere be careful to look up documentation for the right library.

[mtl]: https://www.stackage.org/package/mtl
[transformers]: https://www.stackage.org/package/transformers
