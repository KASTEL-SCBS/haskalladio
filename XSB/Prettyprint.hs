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
            , Token.identStart      = letter
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
  subproofs <- brackets $ notComplex `sepBy` comma
  return $ Node (Not $ Assertion t) []

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
assertion =  parens $ simple <|> notSimple


term :: Parser Term
term = do
  f   <- identifier
  xs  <- option [] $ parens $ term `sepBy` comma
  return $ Node f xs

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



-- sdgParser :: Parser SDGFile
-- sdgParser = whiteSpace >> sdg_file




-- sdg_file :: Parser SDGFile
-- sdg_file = do
--   (version, name) <- sdg_header
--   braces $ do joanaCompiler <- option False ((reserved "JComp") >> (return True))
--               nl <- node_list
--               ti <- option () thread_info
--               return $ SDGFile { name = name, joanaCompiler = joanaCompiler, nodes = nl, version = version }


-- thread_info :: Parser ()
-- thread_info = return ()


-- sdg_header :: Parser (Integer,String)
-- sdg_header = do
--   reserved "SDG"
--   version <- option default_version (reserved "v" >> integer)
--   name    <- option default_sdg_name stringLiteral
--   return (version, name)


-- node_list :: Parser [SDGNodeStub]
-- node_list =  node `sepBy` (return ()) 

-- node :: Parser SDGNodeStub
-- node = do
--   kind <- node_kind
--   id <- integer
--   braces $ do attributes <- node_attributes
--               edges <- node_edges
--               return (id, kind, attributes, edges)

-- node_kind :: Parser SDGNode.Kind
-- node_kind = choice [
--      do reserved kindStr
--         return kind
--      | (kindStr, kind) <- SDGNode.stringToKind
--   ]

-- node_attributes :: Parser [NodeAttribute]
-- node_attributes = do
--   attributes <- (node_attr <* semi) `sepBy` (return ()) 
--   return attributes

-- node_attr :: Parser NodeAttribute
-- node_attr =
--                   (reserved "S" >> node_source)   -- TODO: "defaultSrcPos" umsetzen
--    <|>            (reserved "B" >> node_bytecode) -- TODO: "defaultBcPos" umsetzen
--    <|> (try $ do { reserved "U" ;     id <- integer         ; return $ UnitId id })
--    <|>        do { reserved "P" ; procId <- integer         ; return $ P procId }
--    <|>            (reserved "O" >> node_oper)
--    <|>        do { reserved "V" ;    val <- stringLiteral   ; return $ V val }
--    <|>        do { reserved "T" ;    typ <- stringLiteral   ; return $ T typ }
--    <|>        do { reserved "Z" ;     tn <- may_neg_num_set ; return $ Z tn }
--    <|>        do { reserved "N" ;                           ; return $ N }
--    <|>        do { reserved "C" ;     cl <- stringLiteral   ; return $ C cl }
--    <|>        do { reserved "A" ;     al <- pos_num_set     ; return $ A al }
--    <|>        do { reserved "D" ;     ds <- pos_num_set     ; return $ D ds }
--    <|>        do { reserved "U" ;    uct <- stringLiteral   ; return $ Unresolved uct }




-- pos_num_set :: Parser [Integer]
-- pos_num_set = integer `sepBy` comma


-- may_neg_num_set :: Parser [Integer]
-- may_neg_num_set = integer `sepBy` comma

-- node_source :: Parser NodeAttribute
-- node_source = do
--   filename <- stringLiteral
--   colon
--   startRow <- integer
--   comma
--   startColumn <- integer
--   reservedOp "-"
--   endRow <- integer
--   comma
--   endColumn <- integer
--   return $ S (SPos { filename = filename, start = (startRow, startColumn), end = (endRow, endColumn) })


-- node_bytecode :: Parser NodeAttribute
-- node_bytecode = do
--   name <- stringLiteral
--   colon
--   index <- integer
--   return $ B (BPos { methodName = name, index = index })


-- node_oper :: Parser NodeAttribute
-- node_oper = choice [
--      do reserved operStr
--         return $ O oper
--      | (oper, operStr, _) <- SDGNode.type2info
--   ]


-- node_edges :: Parser [SDGEdgeStub]
-- node_edges = (edge <* semi) `sepBy` (return ()) 

-- edge :: Parser SDGEdgeStub
-- edge = do
--   k <- edge_kind
--   nr <- integer
--   label <- option default_label ( colon >> stringLiteral)
--   return (k, nr, label)

-- edge_kind :: Parser SDGEdge.Type
-- edge_kind =  choice [
--      do reserved edgeStr
--         return edge
--      | (edge, edgeStr, _) <- SDGEdge.type2info
--   ]





-- type ClusterInfo = [([Integer],String)]
-- clusterFromFile :: String -> ClusterInfo
-- clusterFromFile file = unsafePerformIO $
--   do clusters  <- readFile file
--      return $ read clusters
