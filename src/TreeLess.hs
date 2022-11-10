module TreeLess where

import           Data.Bifunctor
import           Lang
import           Utils

treeLess :: Int -> Defs -> Term -> Term
treeLess 0 _ = id
treeLess depth defs = \case
  v@(Var _)            -> v
  Ctor n ts            -> Ctor n (f <$> ts)
  App  n ts            -> f $ let Def {..} = defs n in substs (zip defParams ts) defBody
  Case v@(Var _) ps    -> Case v (fmap f <$> ps)
  Case (Ctor n ts) ps  -> f $ let (PatCtor{..}, t) = ps `patLookup` n in substs (zip patVars ts) t
  Case t@(App _ _) ps  -> f $ Case (f t) ps
  Case (Case t ps) ps' -> f $ Case (f t) $ fmap (second $ flip Case ps') ps
  where
    f = treeLess (depth - 1) defs
