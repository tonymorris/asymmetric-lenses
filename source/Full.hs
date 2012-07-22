data Lens a b = Lens {
  set :: a -> b -> a
, get :: a -> b
}
