-- | Module:    Database.PostgreSQL.Protocol.Internal.Checked
-- Description: Monad for capturing failure
-- Copyright:   © 2015-2020 Patryk Zadarnowski <pat@jantar.org>
-- License:     BSD3
-- Maintainer:  Patryk Zadarnowski «pat@jantar.org»
-- Stability:   experimental
-- Portability: portable

module Database.PostgreSQL.Protocol.Internal.Checked (
  Checked, runChecked, unchecked
) where

-- | A monad transformer which captures 'fail' calls into a user-accessible
-- error message, effectively providing a useful 'MonadFail' instance for an
-- arbitrary monad such as 'Maybe' and '[]'.
newtype Checked m a = Checked { fromChecked :: m (Either String a) }

instance Functor m => Functor (Checked m) where
  fmap f = Checked . fmap (fmap f) . fromChecked

instance Applicative m => Applicative (Checked m) where
  pure = Checked . pure . Right
  mf <*> mx = Checked (fmap (<*>) (fromChecked mf) <*> (fromChecked mx))

instance Monad m => Monad (Checked m) where
  m >>= k = Checked (fromChecked m >>= either (return . Left) (fromChecked . k))

instance Monad m => MonadFail (Checked m) where
  fail = Checked . return . Left

-- | @runChecked h m@ executes the monadic action @m@, passing any 'fail' calls
-- within @m@ with the supplied error handler @h@.
runChecked :: Monad m => (String -> m a) -> Checked m a -> m a
runChecked h (Checked m) = m >>= either h return

-- | Lifts an arbitrary monadic action into the 'Checked' monad.
unchecked :: Functor m => m a -> Checked m a
unchecked = Checked . fmap Right
