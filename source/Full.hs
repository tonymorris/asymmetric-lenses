data Lens a b = Lens {
  set :: a -> b -> a
, get :: a -> b
}

modify ::
  Lens a b
  -> (b -> b)
  -> a
  -> a
modify (Lens set get) m target =
  set target (m (get target))

(>>>) ::
  Lens a b
  -> Lens b c
  -> Lens a c
Lens s1 g1 >>> Lens s2 g2 =
  Lens (\a c -> s1 a (s2 (g1 a) c)) (g2 . g1)

nameLens ::
  Lens Person String
nameLens =
  Lens (\p n -> p { name = n }) name

addressLens ::
  Lens Person Address
addressLens =
  Lens (\a n -> a { address = n }) address

suburbLens ::
  Lens Address String
suburbLens =
  Lens (\a n -> a { suburb = n }) suburb

reverseSuburb ::
  Person
  -> Person
reverseSuburb =
  modify (addressLens >>> suburbLens) reverse

data Address = Address {
  street :: String
, suburb :: String
}

data Person = Person {
  name :: String
, address :: Address
}
