data Lens a b = Lens {
  set :: a -> b -> a
, get :: a -> b
}

modify :: Lens a b -> (b -> b) -> a -> a
modify (Lens set get) m target =
  set target (m (get target))
