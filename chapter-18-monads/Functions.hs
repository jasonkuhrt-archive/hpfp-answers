module Functions where




-- 1
j :: Monad m => m (m a) -> m a
j = (>>= id)



-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
-- Or:
-- l1 = (<$>)
-- l1 f m = m >>= (pure . f)



-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = f <$> m1 <*> m2
-- l2 f m1 m2 = (fmap f m1) >>= (\f' -> fmap f' m2)



-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a m mf = mf >>= (\f -> fmap f m)
-- a m mf = mf >>= ((flip fmap) m)
-- a m mf = mf <*> m



-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _      = pure []
meh (z:zs) f  = f z >>= (\x -> fmap (x:) (meh zs f))

-- Example:
-- <==  meh [1..10] (\x -> Just x)
-- ==>  Just [1,2,3,4,5,6,7,8,9,10]


-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id
