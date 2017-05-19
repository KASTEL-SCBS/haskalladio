module Prettyprint where 


import Prelude
import Data.Maybe (fromJust)
import Data.List (find, intercalate)
import Data.Tree
import qualified Data.Map (lookup, fromList) 

import Text.Read (readMaybe)

import System.IO
import System.IO.Unsafe
import System.Environment(getArgs)

import Control.Monad
import Control.Applicative ((<*),(*>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter <|> char '_' <|> char '\\'
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "length", "justify6","not", "*"]
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
     deriving (Eq)
type Term = Tree Atom


showTerm :: Term -> String
showTerm (Node f []) = f 
showTerm (Node f xs) = f ++ "(" ++ (intercalate "," (fmap showTerm xs)) ++ ")"

instance Show Assertion where
  show (Assertion t) = showTerm t
  show (Not a)       = "not " ++ (show a)
  show (NotEq left right) = "not [" ++ (intercalate "," $ fmap showTerm left)  ++ "] = ["
                                    ++ (intercalate "," $ fmap showTerm right) ++ "]"



type Proof = Tree Assertion
type AnalysisResult = [Proof]

prettyPrint :: String -> String -> IO ()
prettyPrint file descsFile =
  forM_ (fmap drawTree $  fmap (fmap show) (fmap (insertDescriptions descs) $ fromFile file)) putStrLn
 where descs = descriptionFromFile descsFile

insertDescriptions :: [Term] -> Proof -> Proof
insertDescriptions descs proof = fmap f proof
  where f (Assertion t)   = Assertion ((>>=) t g)
        f (Not a)         = Not (f a)
        f (NotEq ts1 ts2) = NotEq (fmap ((=<<) g) ts1) (fmap ((=<<) g) ts2)

        g :: Atom -> Tree Atom
        g atom = case (readMaybe atom :: Maybe Integer) of
          Just x ->
            case Data.Map.lookup x idMap of 
              Just (desc, instanceDesc) -> Node instanceDesc [Node atom [], Node desc []]
              Nothing                   -> Node atom []
          _      -> Node atom []

        idMap = Data.Map.fromList [ (read id, (desc,instanceDesc)) | Node "itemDescription" [Node id [], Node desc [], Node instanceDesc [] ] <- descs]




main = do
       [file, descriptionFile] <- getArgs
       prettyPrint file descriptionFile


descriptionParser :: Parser [ Term ]
descriptionParser = many lineParser
  where lineParser = term
  

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
  left <- parens $ term `sepBy` (reserved "','")
  reservedOp "="
  right <- parens $ term `sepBy` (reserved "','")
  reservedOp "','"
  brackets $ return ()
  return $ Node (NotEq left right) []

assertion :: Parser Proof
assertion =  parens $ try simple <|> notSimple


star :: Parser String
star = do
  reserved "*"
  return "*"

term :: Parser Term
term = try normal <|> list
  where normal = do
                   f   <- identifier <|> (liftM show integer) <|> (char '\'' *> (many $ noneOf ['\'']) <* char '\'') <|> star
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


descriptionFromFile :: String -> [Term]
descriptionFromFile file = unsafePerformIO $ 
  do result  <- readFile file
     case parse descriptionParser "" result of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
