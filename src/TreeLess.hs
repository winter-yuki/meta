module TreeLess where

import           Data.Bifunctor
import           Lang
import           Utils

treeLess :: Decls -> Term -> Term
treeLess decls = \case
  v@(Var _)            -> v
  Ctor n ts            -> Ctor n (treeLess decls <$> ts)
  App  n ts            -> let Decl {..} = decls n in substs declBody (zip declParams ts)
  Case v@(Var _) ps    -> Case v (fmap (treeLess decls) <$> ps)
  Case (Ctor n ts) ps  -> let t = ps `patLookup` n in substs t (zip (patName . fst <$> ps) ts)
  Case t@(App _ _) ps  -> treeLess decls $ Case (treeLess decls t) ps
  Case (Case t ps) ps' -> treeLess decls $ Case t $ fmap (second $ flip Case ps') ps
