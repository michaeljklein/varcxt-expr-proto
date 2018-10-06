# varcxt-expr-proto

Expressions with:

- Contexts modeled by functions
- Variables with ids, stored in contexts
- `Lambda` and `Let` bindings on `Pattern`s

```haskell
newtype Pattern v a b = Pattern
  { runPattern :: forall c d. (a -> Either c b) :=> (V v a -> (V v b :=> Expr d) -> Either c (Expr d))
  }
```


## Expression GADT

```haskell
-- | Expressions with variables, lambda/let binding, and literals
data Expr v a where
  Lit :: a -> Expr v a
  Var :: (V v a :=> Expr v a) :~: t -> Expr v t
  Lambda :: (a -> b) :~: t -> v -> Expr v (V v a :=> Expr v b) -> Expr v t
  Let :: v -> Expr v (V v a :=> Expr v b) -> Expr v a -> Expr v b
  ToCxt :: (a :=> b) :~: t -> Expr v (a -> b) -> Expr v t
  App :: Expr v (a -> b) -> Expr v a -> Expr v b
```


### Evaluation

Evaluate an `Expr`:

```haskell
eval :: Expr v a -> a

λ> eval (Lambda Refl () (Var Refl)) (10 :: Int)
10
```


## Known flaws

The model doesn't work very well.

- There isn't a good way to bind and reference multiple variables
- It's not a `Functor`, and possible type-level benefits would likely
  be lost by adding an explicit `Fmap :: (a -> b) -> Expr v a -> Expr v b`
  constructor
- A closed type family (see below) could be used to distinguish functions
  from contexts from everything else, but using those distinctions to
  provide semeantics seems error prone and/or verbose

```haskell
type family IsCxtType (a :: *) :: Maybe Bool where
  CxtType (a -> b) = 'Just 'False
  CxtType (a :=> b) = 'Just 'True
  CxtType _ = 'Nothing
```


# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/varcxt-expr-proto/)

