module TreeLess where

import           Data.Bifunctor
import           Lang
import           Utils

treeLess :: Int -> Decls -> Term -> Term
treeLess 0 _ = id
treeLess depth decls = \case
  v@(Var _)            -> v
  Ctor n ts            -> Ctor n (f <$> ts)
  App  n ts            -> f $ let Decl {..} = decls n in substs declBody (zip declParams ts)
  Case v@(Var _) ps    -> Case v (fmap f <$> ps)
  Case (Ctor n ts) ps  -> f $ let t = ps `patLookup` n in substs t (zip (patName . fst <$> ps) ts)
  Case t@(App _ _) ps  -> f $ Case (f t) ps
  Case (Case t ps) ps' -> f $ Case (f t) $ fmap (second $ flip Case ps') ps
  where
    f = treeLess (depth - 1) decls
