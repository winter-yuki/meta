module Utils where

import           Data.List  (find)
import           Data.Maybe
import           Lang

type Subst = (Ident, Term)

subst :: Subst -> Term -> Term
subst = substs . (: [])

substs :: [Subst] -> Term -> Term
substs ss = \case
  v@(Var n)  -> fromMaybe v (lookup n ss)
  App n' ts  -> App n' (substs ss <$> ts)
  Ctor n' ts -> Ctor n' (substs ss <$> ts)
  Case t' ps -> Case (substs ss t') (fmap (substs ss) <$> ps)

patLookup :: [(Pattern, Term)] -> Ident -> (Pattern, Term)
patLookup ps n =
  fromMaybe (error "Pattern not found :(") $
  find (\(PatCtor n' _, _) -> n == n') ps
