module Utils where

import           Data.List
import           Data.Maybe
import           Lang

type Subst = (Ident, Term)

data Decl = Decl { declParams :: [Ident], declBody :: Term }
type Decls = Ident -> Decl

subst :: Subst -> Term -> Term
subst s@(n, t) = \case
  Var n' | n == n' -> t
  App n' ts        -> App n' (subst s <$> ts)
  Ctor n' ts       -> Ctor n' (subst s <$> ts)
  Case t' ps       -> Case (subst s t') (fmap (subst s) <$> ps)

substs :: Term -> [Subst] -> Term
substs = foldr subst

patLookup :: [(Pattern, Term)] -> Ident -> Term
patLookup ps n = snd $
  fromMaybe (error "Pattern not found :(") $
  find (\(PatCtor n' _, _) -> n == n') ps
