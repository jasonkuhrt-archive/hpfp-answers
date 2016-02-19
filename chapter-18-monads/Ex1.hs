module Ex1 where


import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind mf mx = join (fmap mf mx)
