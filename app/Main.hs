module Main (main) where

import           Lang
import           Parser
import           System.Environment
import           Text.Parsec
import           TreeLess

main :: IO ()
main = getArgs >>= \case
  [mode] | mode == "-p" -> print testTerm
  [declsPath, mode, name] | mode == "-p" -> printDecl name declsPath
  [declsPath, mode, depth, name] | mode == "-d" -> deforesterizate name (read depth) declsPath
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

process :: Show a => (Decls -> a) -> FilePath -> IO ()
process f declsPath = do
  mbDecls <- runParser parseFile () declsPath <$> readFile declsPath
  case mbDecls of
    Left e      -> print e
    Right decls -> print $ f decls

printDecl :: Ident -> FilePath -> IO ()
printDecl ident = process ($ ident)

deforesterizate :: Ident -> Int -> FilePath -> IO ()
deforesterizate ident depth = process $ \decls ->
  let d@Decl{..} = decls ident in
  d { declBody = treeLess depth decls declBody }
