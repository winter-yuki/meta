module Main (main) where

import           Lang
import           Parser
import           System.Environment
import           Text.Parsec
import           TreeLess

main :: IO ()
main = getArgs >>= \case
  [mode] | mode == "-p" -> print testTerm
  [defsPath, mode, name] | mode == "-p" -> printDef name defsPath
  [defsPath, mode, depth, name] | mode == "-d" -> deforestate name (read depth) defsPath
  _ -> error "Wrong arguments format"

testTerm :: Term
testTerm =
  Case
    (Case
      (Var "t1")
      [ (PatCtor "c1" ["v1", "v2"], App "f" [Var "arg1", Var "arg2"])
      , (PatCtor "c2" ["v1"], Ctor "c3" [App "g" [Var "arg3"], Var "arg4"]) ])
    [ ( PatCtor "c4" ["v5", "v6"]
      , Case
          (App "h" [Var "a", Var "b", Var "c"])
          [ (PatCtor "c4" ["v5", "v6"], Var "p") ] ) ]

process :: Show a => (Defs -> a) -> FilePath -> IO ()
process f defsPath = do
  mbDefs <- runParser parseFile () defsPath <$> readFile defsPath
  case mbDefs of
    Left e      -> print e
    Right defs -> print $ f defs

printDef :: Ident -> FilePath -> IO ()
printDef ident = process ($ ident)

deforestate :: Ident -> Int -> FilePath -> IO ()
deforestate ident depth = process $ \defs ->
  let d@Def{..} = defs ident in
  d { defBody = treeLess depth defs defBody }
