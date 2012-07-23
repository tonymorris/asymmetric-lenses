import Data.Map
import Data.Set
import qualified Data.Map as M
import qualified Data.Set as S

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

(|||) ::
  Lens a x
  -> Lens b x
  -> Lens (Either a b) x
Lens s1 g1 ||| Lens s2 g2 =
  Lens 
    (either (\a -> Left . s1 a) (\b -> Right . s2 b))
    (either g1 g2)

(***) ::
  Lens a b
  -> Lens c d
  -> Lens (a, c) (b, d)
Lens s1 g1 *** Lens s2 g2 =
  Lens  
    (\(a, c) (b, d) -> (s1 a b, s2 c d))
    (\(a, c) -> (g1 a, g2 c))

unzipL ::
  Lens s (a, b)
  -> (Lens s a, Lens s b)
unzipL (Lens s g) =
  (
    Lens (\t a -> s t (a, snd (g t))) (fst . g)
  , Lens (\t b -> s t (fst (g t), b)) (snd . g)
  )

factor ::
  Lens
    (Either (a, b) (a, c))
    (a, Either b c)
factor =
  Lens
    (\e (a, ee) -> either (\b -> Left (a, b)) (\c -> Right (a, c)) ee)
    (either (\(a, b) -> (a, Left b)) (\(a, c) -> (a, Right c)))

distribute ::
  Lens
    (a, Either b c)
    (Either (a, b) (a, c))
distribute =
  Lens
    (\_ -> either (\(aa, bb) -> (aa, Left bb)) (\(aa, cc) -> (aa, Right cc)))
    (\(a, e) -> either (\b -> Left (a, b)) (\c -> Right (a, c)) e)

fstLens ::
  Lens (a, b) a
fstLens =
  Lens (\(_, b) a -> (a, b)) fst

sndLens ::
  Lens (a, b) b
sndLens =
  Lens (\(a, _) b -> (a, b)) snd

mapLens ::
  Ord k =>
  k
  -> Lens (Map k v) (Maybe v)
mapLens k =
  Lens
    (\m -> maybe (M.delete k m) (\v' -> M.insert k v' m))
    (M.lookup k)

setLens ::
  Ord a =>
  a
  -> Lens (Set a) Bool
setLens a =
  Lens
    (\s p -> (if p then S.insert else S.delete) a s) 
    (S.member a)

data PartialLens a b = PartialLens (a -> Maybe (b -> a, b))

pmodify ::
  PartialLens a b
  -> (b -> b)
  -> a
  -> Maybe a
pmodify (PartialLens f) m target =
  fmap (\(s, g) -> s (m g)) (f target)

(>>>>) ::
  PartialLens a b
  -> PartialLens b c
  -> PartialLens a c
PartialLens f >>>> PartialLens g =
  PartialLens (\a -> 
    do (x, b) <- f a 
       (y, c) <- g b
       return (x . y, c))

jArrayLens ::
  PartialLens Json [Json]
jArrayLens =
  PartialLens
    (\j -> case j of 
      JArray x -> Just (JArray, x)
      _ -> Nothing)

jHeadLens ::
  PartialLens [a] a
jHeadLens =
  PartialLens
    (\x -> case x of
      (h:t) -> Just (\i -> i:t, h)
      _ -> Nothing)

data Json =
  JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [Json]
  | JObject [(String, Json)]

(||||) ::
  PartialLens a x
  -> PartialLens b x
  -> PartialLens (Either a b) x
PartialLens f |||| PartialLens g =
  PartialLens 
    (either 
      (fmap (\(p, q) -> (Left . p, q)) . f) 
      (fmap (\(p, q) -> (Right . p, q)) . g))

(****) ::
  PartialLens a b
  -> PartialLens c d
  -> PartialLens (a, c) (b, d)
PartialLens f **** PartialLens g =
  PartialLens
    (\(a, c) -> do (b, b') <- f a
                   (d, d') <- g c
                   return (\(t, u) -> (b t, d u), (b', d')))

punzipL ::
  PartialLens s (a, b)
  -> (PartialLens s a, PartialLens s b)
punzipL (PartialLens f) =
  (
    PartialLens (fmap (\(a, (p, q)) -> (\k -> a (k, q), p)) . f)
  , PartialLens (fmap (\(b, (p, q)) -> (\k -> b (p, k), q)) . f)
  )

pfactor ::
  PartialLens 
    (Either (a, b) (a, c))
    (a, Either b c)
pfactor =
  PartialLens
    (Just . \e -> 
      (
        \(a, ee) -> either (\b -> Left (a, b)) (\c -> Right (a, c)) ee
      , either (\(a, b) -> (a, Left b)) (\(a, c) -> (a, Right c)) e
      ))

pdistribute ::
  PartialLens
    (a, Either b c)
    (Either (a, b) (a, c))
pdistribute =
  PartialLens
    (Just . \(a, e) -> 
      (
        either (\(aa, bb) -> (aa, Left bb)) (\(aa, cc) -> (aa, Right cc))
      , either (\b -> Left (a, b)) (\c -> Right (a, c)) e
      ))

