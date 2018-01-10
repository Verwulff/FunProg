newtype PSet a = PSet{ contains :: (a -> Bool) }
instance Monoid (PSet a) where
  mempty = PSet (\x -> False)
  mappend (PSet x) (PSet y) = PSet (\c -> x c || y c)


newtype PSet' a = PSet'{ contains' :: (a -> Bool) }
instance Monoid (PSet' a) where
  mempty = PSet' (\x -> False)
  mappend (PSet' x) (PSet' y) = PSet' (\c -> x c && y c)


newtype PSet'' a = PSet''{ contains'' :: (a -> Bool) }
instance Monoid (PSet'' a) where
  mempty = PSet'' (\x -> False)
  mappend (PSet'' x) (PSet'' y) = PSet'' (\c -> (x c && (not $ y c)) || ((not $ x c) && y c))


instance Functor PSet where
  fmap _ _ = PSet (\_ -> False)

instance Functor PSet' where
fmap _ _ = PSet' (\_ -> True)
