
# Journey of a Haskell expression

The expression:

```haskell
  traverse (Constant . (+1)) ([Sum 1, Sum 1] :: [Sum Int])
  -- (== Constant (Sum {getSum = 4}))
```

`traverse` is a method from the Traversable Typeclass. Its type is:

```haskell
  (Applicative f, Traversable t) =>
    (a -> f b) -> t a -> f (t b)
```

As we can see above, the traversable is `t`. When `traverse` executes Haskell will dispatch to the implementation provided by the concrete type that `t` has become. In our expression `t` has become the List Type. Its implementation of the Traversable Typeclass is:

```haskell
  instance Traversable [] where
    traverse f = List.foldr cons_f (pure []) -- NOTE eta reduce
    where
    cons_f x ys = (:) <$> f x <*> ys
```

The implementation depends upon methods from both the Functor Typeclass (`<$>` operator which is just an alias to `fmap` function) and the Applicative Typeclass (`pure`, `<*>`). A refresher on these methods:

* `fmap`
```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```
`fmap` lifts a unary function (one argument function) over a structure in order that it then can be applied to the value(s) therein.

* Applicative apply
```haskell
(<*>) :: Applicative a => a (x -> z) -> a x -> a z
```
Applicative apply is `fmap` except the function is inside structure (note: as a result of being within structure there may be multiple functions e.g. if the structure is a list). It lifts (a) unary function(s) out of its/their structure and then over another structure in order that it/they then can be applied to the value(s) therein.

* `pure`
```haskell
pure :: Applicative a => x -> a x
```
`pure` just makes an applicative value. For example `(pure 1 :: Maybe Integer) == Just 1`. Note the explicit concrete type; Otherwise Haskell would not know which implementation to use for `pure`.

Now the implementation of `traverse` here does: Seed `foldr` with an embedded list. Apply the given function `f` to each value `x` in the list. On each iteration use `fmap (AKA <$>)` to transform what `f` returns from `x` in structure into prepend-x-to-list (a function) in structure. Finally use Applicative apply (`<*>`) to apply the embedded function to the embedded list which will result in an embedded list again but now with that iterations `x` prepended to it.

#### Constant Type implementations

The `f` that `traverse` uses becomes `(Constant . (+1))` in our expression. The extra structure produced by `f` is the `Constant` structure. The `<$>` and `<*>` methods dispatch to the implementations provided by the `Constant Type`. `fmap` is implemented to return the embedded value as-is (which, for a constant, makes sense). Applicative Apply is defined notably different from other well known Types like Maybe, Either, List in that rather than requiring a function and value embedded in a respective structure, it requires two *monoids* (this is the difference) embedded in Constant which it then joins together via `mappend` (AKA Monoid Append).

#### Understanding Sum

`List traverse` dispatches to `Constant <*>` dispatches to `Sum mappend` which uses `+`. TODO fully explain.

#### Combine understanding of respective List / Constant / Sum methods at play

So, in our expression, as List's `traverse` method executes, its implementation dispatches to Constant's methods. The key moments are:

1. `pure []` results in Concrete Type `Constant (Sum Int) [a]` to seed accumulator.

2. `f x` adds 1 to `x` and embeds it into a Constant structure. `Constant (Sum Int) a`

3. `fmap (:)` results in no change to the value embedded inside Constant, but does change the type into  `Constant (Sum Int) ([a] -> [a])`.

4. `<*>` appends the result of step 3 to the accumulator. The result of this is used for the next iteration (if there is one).
