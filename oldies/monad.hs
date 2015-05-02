module Main where

class MyMonad m where
    (~>>=)      :: m a -> (a -> m b) -> m b
    (~>>)       :: m a -> m b -> m b
    give        :: a -> m a
    die         :: String -> m a

    m ~>> k     = m ~>>= \_ -> k
    die s       = error s

instance MyMonad ((->) t) where
    give :: a -> t -> a
    give = const
    (~>>=) :: (t -> a) -> (a -> (t -> b)) -> (t -> b)
    f ~>>= k = \r -> k (f r) r


newtype Foo a = Foo a deriving Show
data Option a = Some a | None

instance Monad Option where
    (>>=) :: Option a -> (a -> Option b) -> Option b
    (Some v) >>= f = f v
    None     >>= f = None

    return :: a -> Option a
    return = Some
    fail _ = None

instance Monad Foo where
    (Foo a) >>= f = f a
    return = Foo


sumFromTo :: (Num a, Enum a) => a -> a -> Foo a
sumFromTo a b = do
    seq <- return [a..b]
    v <- return $ foldr (+) 0 seq
    return v


