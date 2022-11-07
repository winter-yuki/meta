module Main (main) where

import           Lang

main :: IO ()
main = print $
  Case
    (Case
      (Var "t1")
      [ (PatCtor "c1" ["v1", "v2"], App "f" [Var "arg1", Var "arg2"])
      , (PatCtor "c2" ["v1"], Ctor "c3" [App "g" [Var "arg3"], Var "arg4"]) ])
    [ ( PatCtor "c4" ["v5", "v6"]
      , Case
          (App "h" [Var "a", Var "b", Var "c"])
          [ (PatCtor "c4" ["v5", "v6"], Var "p") ] ) ]
