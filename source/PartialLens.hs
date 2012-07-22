data PartialLens a b = PartialLens {
  set :: a -> Maybe (b -> a)
, get :: a -> Maybe b
}
