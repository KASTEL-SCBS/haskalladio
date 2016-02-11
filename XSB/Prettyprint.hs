module Prettyprint where 


import Prelude
import Data.Maybe (fromJust)
import Data.List (find, intercalate)
import Data.Tree

import System.IO
import System.IO.Unsafe
import System.Environment(getArgs)

import Control.Monad
import Control.Applicative ((<*))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter <|> char '_'
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "length", "justify6","not"]
            , Token.reservedOpNames = ["','", "=" ]
            }


lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
braces     = Token.braces     lexer
brackets   = Token.brackets   lexer
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
stringLiteral = Token.stringLiteral lexer
comma         = Token.comma   lexer
colon         = Token.colon   lexer



type Atom = String
data Assertion = Assertion Term
               | Not Assertion
               | NotEq [Term] [Term]

type Term = Tree Atom


showTerm :: Term -> String
showTerm (Node f []) = f 
showTerm (Node f xs) = f ++ "(" ++ (intercalate "," (fmap showTerm xs)) ++ ")"

instance Show Assertion where
  show (Assertion t) = showTerm t
  show (Not a)       = "not " ++ (show a)



type Proof = Tree Assertion
type AnalysisResult = [Proof]

prettyPrint :: String -> IO ()
prettyPrint file =
  forM_ (fmap drawTree $  fmap (fmap show) (fromFile file)) putStrLn


main = do
       [file] <- getArgs
       prettyPrint file

resultParser :: Parser AnalysisResult
resultParser = do
    reserved "justify6"
    colon
    result <- brackets $ do assertion `sepBy` (reservedOp ",")
    reserved "length"
    colon
    integer
    return result


simple :: Parser Proof
simple = do
  t <- term
  reservedOp "','"
  subproofs <- brackets $ assertion `sepBy` comma
  return $ Node (Assertion t) subproofs

notSimple :: Parser Proof
notSimple = do
  reserved "not"
  t <- term
  reservedOp "','"
  subproofs <- brackets $ (try notComplex <|> assertion) `sepBy` comma
  return $ Node (Not $ Assertion t) subproofs

notComplex :: Parser Proof
notComplex = parens $ do
  reserved "not"
  parens $ term `sepBy` (reserved "','")
  reservedOp "="
  parens $ term `sepBy` (reserved "','")
  reservedOp "','"
  brackets $ return ()
  return $ Node (NotEq [] []) []

assertion :: Parser Proof
assertion =  parens $ try simple <|> notSimple


term :: Parser Term
term = try normal <|> list
  where normal = do
                   f   <- identifier <|> (liftM show integer)
                   xs  <- option [] $ parens $ term `sepBy` comma
                   return $ Node f xs
        list   = do
                   terms <- brackets $ term `sepBy` comma
                   return $ Node "list" terms

parseString :: String -> AnalysisResult
parseString str =
  case parse resultParser "" str of
    Left e  -> error $ show e
    Right r -> r
 
parseFile :: String -> IO AnalysisResult
parseFile file =
  do result  <- readFile file
     case parse resultParser "" result of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

fromFile :: String -> AnalysisResult
fromFile file = unsafePerformIO $ 
  do result  <- readFile file
     case parse resultParser "" result of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
