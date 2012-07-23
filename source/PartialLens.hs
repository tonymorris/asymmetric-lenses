data PartialLens a b =
  PartialLens (a -> Maybe (b -> a, b))
