# Generics

Generics allow us to write Haskell code that can work with any type. An example is the `aeson` library, which is able to automatically create JSON encoders and decoders for (almost) all Haskell types. How do they do it?

As Haskell developers we're able to define our own type. There's no way the author of a library like `aeson` is able to write out separate logic for every single type users of the library might come up with. Instead `aeson` makes use of `GHC.Generics`.

`GHC.Generics` provides an alternative representation of types and values. We can convert our regular types and values into this alternative representation and back. The cool thing is that in this alternative representation we can express all types using 6 basic building blocks. This allows us to write functions that work for any type, like so:

- A function that accepts any type as an argument first converts that argument to the alternative representation, using `GHC.Generics.from`. The function then implements behavior for each of the 6 possible building blocks.
- A function that returns any type constructs its return value using the 6 basic building blocks, then converts that value to its 'real' representation using `GHC.Generics.to`.
- A function can combine the above two approaches to both accept and return arguments of any type.

We're now going to dive straight in and learn about these building blocks by looking at the alternative representations of a couple of example types. For each type we're going to compare the original Haskell type and an example value of that type to the alternative representation of the original type and the alternative representation of the example value.

|             | Type                                              | Example value |
| ----------- | ------------------------------------------------- | ------------- |
| original    | newtype Id = Id Int                               | Id 5          |
| alternative | data Person = Person { name :: Text, age :: Int } |               |
