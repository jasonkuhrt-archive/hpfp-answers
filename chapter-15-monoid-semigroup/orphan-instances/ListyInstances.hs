module ListyInstances where

import Data.Monoid
import Listy



instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy x) (Listy z) = Listy (mappend x z)
