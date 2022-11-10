module Lang
  ( Ident
  , Pattern(..)
  , Term(..)
  , Def(..)
  , Defs
  ) where

import           Data.Functor
import           Data.List    (intercalate)

type Ident = String

data Pattern = PatCtor { patName :: Ident, patVars :: [Ident] }
  deriving (Eq)

instance Show Pattern where
  show PatCtor {..} = unwords (patName : patVars)

data Term =
    Var Ident
  | App Ident [Term]
  | Ctor Ident [Term]
  | Case Term [(Pattern, Term)]
  deriving (Eq)

instance Show Term where
  show = showIndent 0

isShort :: Term -> Bool
isShort (Var _) = True
isShort _       = False

showIndent :: Int -> Term -> String
showIndent i = \case
  Var n     -> n
  App n ts  -> appAndCtor i n ts
  Ctor n ts -> appAndCtor i n ts
  Case t ps ->
    "\\case " <> showIndent (i + 2) t <> " \\of\n" <>
    indent (i + 2) <> intercalate ("\n" <> indent i <> "| ")
    (ps <&> \(p, t) -> show p <> " -> " <> showIndent (i + 4) t) <>
    "\n" <> indent i <> "\\esac"

appAndCtor :: Int -> Ident -> [Term] -> String
appAndCtor i n ts
  | all isShort ts = unwords (n : fmap show ts)
  | otherwise = n <> "\n" <> indent (i + 2) <> "(" <>
    intercalate (")\n" <> indent (i + 2) <> "(") (showIndent (i + 2) <$> ts) <> ")"

indent :: Int -> String
indent = flip replicate ' '

data Def = Def
  { defName   :: Ident
  , defParams :: [Ident]
  , defBody   :: Term
  }

instance Show Def where
  show Def{..} =
    "\\fun " <> defName <> " " <> unwords defParams <> " => " <>
    showIndent 2 defBody

type Defs = Ident -> Def
