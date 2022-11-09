module Parser
  ( parseTerm
  , parseDecl
  , parseDecls
  , parseFile
  ) where

import           Control.Applicative (liftA2)
import           Data.Char
import           Data.Functor
import           Lang
import           Text.Parsec

type Parser a = Parsec String () a

spaces1 :: Parser ()
spaces1 = void space <* spaces

enclosed :: Parser a -> Parser a
enclosed = between (char '(') (char ')')

anything :: Parser ()
anything = void $
  alphaNum <|> char '\'' <|> char '\\' <|> char '_' <|>
  char '-' <|> char '>' <|> char '|' <|> char '(' <|> char ')' <|> space

ident :: Parser Char -> Parser Ident
ident first = liftA2 (:) firstChar (many nonFirstChars)
  where
    firstChar = first <|> char '_'
    nonFirstChars = alphaNum <|> char '_' <|> char '\''

lident :: Parser Ident
lident = ident (satisfy isLower)

uident :: Parser Ident
uident = ident (satisfy isUpper)

inspaces :: Parser a -> Parser a
inspaces = between spaces spaces

pat :: Parser Pattern
pat = PatCtor <$> uident <*> many (try $ spaces *> lident)

onecase :: Parser (Pattern, Term)
onecase = (,) <$> pat <* inspaces (string "->") <*> term

caseof :: Parser Term
caseof = Case <$
  string "\\case" <*> inspaces term <* string "\\of" <* spaces <*>
  inspaces onecase `sepBy` char '|' <* spaces <*
  string "\\esac"

param :: Parser Term
param = enclosed (inspaces term) <|> var

params :: Parser [Term]
params = many $ param <* spaces

ctor :: Parser Term
ctor = Ctor <$> uident <* spaces <*> params

func :: Parser Term
func = App <$> lident <* spaces <*> liftA2 (:) (param <* spaces) params

var :: Parser Term
var = Var <$> lident

term :: Parser Term
term = enclosed (inspaces term) <|> caseof <|> ctor <|> try func <|> var

parseTerm :: Parser Term
parseTerm = term

decl :: Parser Decl
decl = Decl <$
  string "\\fun" <* spaces <*> lident <* spaces <*> many (inspaces lident) <*
  spaces <* string "=>" <*>
  inspaces term

parseDecl :: Parser Decl
parseDecl = decl

comment :: Parser ()
comment = void $ (string "-- " *> many anything) `endBy1` char '\n'

nondecl :: Parser ()
nondecl = void $ comment <|> spaces1

decls :: Parser Decls
decls = fmap mkDecls $ inspaces $ many (many nondecl *> decl <* many nondecl)
  where
    mkDecls :: [Decl] -> Decls
    mkDecls = foldr
      (\d@Decl{..} ds ident -> if ident == declName then d else ds ident)
      (error "No such declaration found")

parseDecls :: Parser Decls
parseDecls = decls

parseFile :: Parser Decls
parseFile = decls <* eof
