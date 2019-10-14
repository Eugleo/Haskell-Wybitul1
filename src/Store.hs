{-# LANGUAGE DeriveFunctor #-}

module Store where

class Functor w =>
      Comonad w
  where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: w a -> (w a -> b) -> w b
  extend wa f = f <$> duplicate wa

data Store state a =
  Store (state -> a)
        state
  deriving (Functor)

instance Comonad (Store s) where
  extract (Store test s) = test s
  duplicate (Store test s) = Store (Store test) s

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment w (Store f s) = f <$> w s

restore :: Store s a -> s
restore (Store f s) = s

seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s
