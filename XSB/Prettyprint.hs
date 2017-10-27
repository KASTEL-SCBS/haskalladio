{-# LANGUAGE ScopedTypeVariables #-}
module Prettyprint where 


import Prelude
import Data.Char(chr)
import Data.Maybe (fromJust)
import Data.List (find, intercalate, nub, intersperse)
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
            , Token.reservedNames   = [ "length", "justify6","not", "*", "WORLD"]
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
               | AbbreviatedProof Term Int
               | Not Assertion
               | NotEq [Term] [Term]
               | World World
     deriving (Eq)
type Term = Tree Atom


showTerm :: Term -> String
showTerm (Node f []) = f 
showTerm (Node f xs) = f ++ "(" ++ (intercalate "," (fmap showTerm xs)) ++ ")"

instance Show Assertion where
  show (World world) = showWorld Constraints world
  show (Assertion t) = showTerm t
  show (AbbreviatedProof t n) = "(" ++ (show n) ++ " proofs) " ++ showTerm t
  show (Not a)       = "not " ++ (show a)
  show (NotEq left right) = "not [" ++ (intercalate "," $ fmap showTerm left)  ++ "] = ["
                                    ++ (intercalate "," $ fmap showTerm right) ++ "]"


toNthLevel :: Integer -> Tree a -> Tree a
toNthLevel 0 (Node x  _)  = Node x []
toNthLevel n (Node x ts) = Node x $ fmap (toNthLevel (n-1)) ts



-- Funtcions for interactive proof exploration in the ghci repl.
-- Example:
-- λ> let { root = Node (Assertion $ Node "x" []) trees ; trees = fromFile "queries-justify.result" ; descs = descriptionFromFile "descriptions.result" }
--
-- λ> showNode descs [] trees
-- [0] (50 proofs) x
-- |
-- `- [0] (50 proofs) adversary(35,cloud end user A)

-- [1] (75 proofs) x
-- |
-- `- [0] (75 proofs) adversary(36,cloud end user B)

-- [2] (75 proofs) x
-- |
-- `- [0] (75 proofs) adversary(37,cloud end user guest)

-- [3] (80 proofs) x
-- |
-- `- [0] (80 proofs) adversary(38,cloud service administrator)

-- [4] (345 proofs) x
-- |
-- `- [0] (345 proofs) adversary(39,cloud service provider)

-- [5] (165 proofs) x
-- |
-- `- [0] (165 proofs) adversary(40,ChuckNorris)
--
-- λ> showNode descs [(4,0),(0,1),(325,0),(0,2)] trees
-- [0] x
-- |
-- `- adversary(39,cloud service provider)
--    |
--    +- attacker(adversary(39,cloud service provider))
--    |
--    `- isInSecureWithRespectTo(adversary(39,cloud service provider))
--       |
--       +- accessibleParameters(adversary(39,cloud service provider),parm(operationSignature(139,log),parameter(140,text)),assemblyContext(254,LBAdminTool))
--       |  |
--       |  +- containersFullyAccessibleBy(adversary(39,cloud service provider),resourceContainer(490,VM E))
--       |  |
--       |  +- interfacesOn(resourceContainer(490,VM E),operationInterface(59,LogWriter),assemblyContext(254,LBAdminTool))
--       |  |
--       |  `- (1 proofs) parametersOf(operationSignature(139,log),parm(operationSignature(139,log),parameter(140,text)))
--       |     |
--       |     `- [0] (1 proofs) hasParameter(operationSignature(139,log),parm(operationSignature(139,log),parameter(140,text)))
--       |
--       `- not parameterAllowedToBeAccessedBy(adversary(39,cloud service provider),parm(operationSignature(139,log),parameter(140,text)),assemblyContext(254,LBAdminTool))

selectNode []     proofs = [ Node (withCount n x)
                                      [ Node (withCount m y) [] | pp@(Node y []) <- ps, let m =  length $ nub $ [ ps'' | ps' <- cutProofs2, (Node y' ps'' ) <- ps', y == y' ]
                                     ]
                              | Node x ps <- nub cutProofs, let n = length $ filter (== Node x ps) cutProofs, let cutProofs2 = cutProofs2For ps ]
  where withCount n (Assertion a) = AbbreviatedProof a n
        withCount n a                            = a
        cutProofs  = fmap (toNthLevel 1) proofs
        cutProofs2For ps = [ ps' | (Node _ ps')  <- proofs, fmap (toNthLevel 0) ps' == ps]
selectNode ((proofIndex, subGoalIndex):is) proofs = [ Node x (pre ++ [subproof] ++ post) | subproof <- selectNode is [ subproofs !! subGoalIndex |  (Node _ subproofs)  <- relevantProofs] ]
  where proofIndexRepresentant@(Node x ps) = (nub (fmap (toNthLevel 1) proofs)) !! proofIndex
        pre  = take subGoalIndex ps
        post = reverse $ take (length ps - subGoalIndex - 1) (reverse ps)
        relevantProofs                      = [ proof | proof <- proofs, toNthLevel 1 proof == proofIndexRepresentant]

showNode :: [Term] -> [(Int,Int)] -> [Proof] -> IO ()
showNode descs ns proofs = do
   forM_ (fmap drawTree $ fmap withLeafIndices $ zipWith withIndex [0..] $ fmap (fmap show) $  selected) putStrLn
  where processed = [ insertDescriptions descs $ interestingOnly $  termSubst flattenWorld $ proof  | proof <- proofs]
        selected = selectNode ns (fmap (\p -> Node (Assertion $ Node "x" []) [p]) processed)
        withIndex i (Node x xs) = Node ("[" ++ show i ++ "] " ++ x) xs

        withLeafIndices (Node x xs)
            | all isLeaf xs = Node x $ zipWith withLeafIndex [0..] xs
            | otherwise     = Node x $ fmap withLeafIndices xs

        withLeafIndex i (Node x []) = Node ("[" ++ show i ++ "] " ++ x) []
        withLeafIndex i (Node x xs)
            | all isLeaf xs = Node x $ zipWith withLeafIndex [0..] xs
            | otherwise     = Node x $ fmap withLeafIndices xs

        isLeaf (Node _ []) = True
        isLeaf _           = False

        flattenWorld w@(Node "world" _) = Node (showWorld Subs w) []
        flattenWorld x                  = x

showNodeFull :: [Term] ->  Proof -> IO ()
showNodeFull descs root = do
    putStrLn $ drawTree $ (fmap show) $ (insertDescriptions descs $ interestingOnly $ root)


type Proof = Tree Assertion

type AnalysisResult = [(World,[Proof])]
type World = Term



tFilter :: (a -> Bool) -> Tree a -> Tree a
tFilter f (Node x ts)
 | f x = Node x (fmap (tFilter f) ts)
 | otherwise = (Node x [])

termFilter :: (Atom -> Bool) -> Proof -> Proof
termFilter f (Node (World world)               ns) = Node (World world                          ) (fmap (termFilter f) ns)
termFilter f (Node (Assertion        term    ) ns) = Node (Assertion        (tFilter f term))     (fmap (termFilter f) ns)
termFilter f (Node (AbbreviatedProof term int) ns) = Node (AbbreviatedProof (tFilter f term) int) (fmap (termFilter f) ns)
termFilter f (Node (Not assertion            ) ns) =let (Node result ns') = termFilter f (Node assertion ns) in
                                                     Node (Not result) ns'
termFilter f (Node (NotEq terms1 terms2      ) ns) = Node (NotEq (fmap (tFilter f) terms1)
                                                                 (fmap (tFilter f) terms2)
                                                          )                                       (fmap (termFilter f) ns)

tSubst :: (Term -> Term) -> Term -> Term
tSubst subst (Node x ts) = subst (Node x (fmap (tSubst subst) ts))

termSubst :: (Term -> Term) -> Proof -> Proof
termSubst subst proof = fmap f proof
  where f (World world)            = World world -- World ((>>=) world g)
        f (Assertion t)            = Assertion (tSubst subst t)
        f (AbbreviatedProof t n)   = AbbreviatedProof (tSubst subst t) n
        f (Not a)         = Not (f a)
        f (NotEq ts1 ts2) = NotEq (fmap (tSubst subst) ts1) (fmap (tSubst subst) ts2)


interestingOnly :: Proof -> Proof
interestingOnly = (tFilter noFreeIncludesNots) . (termFilter noWorlds)
  where noFreeIncludesNots node@(Not (Assertion (Node "includes" [Node _ _, Node ('_':_) _, Node _ _]))) = False
        noFreeIncludesNots _ = True
        noWorlds "world" = False
        noWorlds _       = True

prettyPrint :: String -> String -> IO ()
prettyPrint file descsFile =
  forM_ worlds (\(world,proofs) -> do
      -- putStrLn $ "WORLD: " ++ (drawTree $  fmap show  $ world)
      putStrLn $ "WORLD:" ++ (showWorld Constraints world)
      forM_ proofs (\proof -> do
          putStrLn $ drawTree $  fmap show  $ insertDescriptions descs $ interestingOnly $ termSubst flattenWorld $ proof
       )
   )
 where descs = descriptionFromFile descsFile
       worlds = fromFile file
       flattenWorld w@(Node "world" _) = Node (showWorld Subs w) []
       flattenWorld x                  = x

data Which = Subs | Constraints deriving Show
showWorld :: Which -> Term -> String
showWorld which (Node "world" [Node "tuple" [ Node "list" subs, Node "list" constraints ]]) = intercalate ", " $ fmap showSub toShow
  where toShow = case which of { Subs -> subs ; Constraints -> constraints }
        showSub (Node "tuple"
                  [Node "tuple" [Node context [], Node interface [], Node parameter []],
                   Node "list" letters
                  ]
                ) = "(" ++ context ++ ", " ++ interface ++ ", " ++ parameter ++ ") ↦ " ++ (fmap toLetter letters)
        showSub (Node "tuple"
                  [Node "tuple" [Node context [], Node interface [], Node parameter []],
                   Node freeVar@('_':_) []
                  ]
                ) = "(" ++ context ++ ", " ++ interface ++ ", " ++ parameter ++ ") ↦ " ++ freeVar
        showSub (Node "tuple"
                  [Node "tuple" [Node context [], Node providedRequired [Node interface []], Node parameter []],
                   Node freeVar@('_':_) []
                  ]
                ) = "(" ++ context ++ ", " ++ providedRequired ++ "(" ++ interface ++ ")" ++ ", " ++ parameter ++ ") ↦ " ++ freeVar
        showSub (Node "tuple"
                  [Node "tuple" [Node context [], Node providedRequired [Node interface []], Node parameter []],
                   Node "list" list
                  ]
                ) = case list of freeVars@((Node freeVar@('_':_) []):moreVars) -> "(" ++ context ++ ", " ++ providedRequired ++ "(" ++ interface ++ ")" ++ ", " ++ parameter ++ ") ↦ " ++ (showList freeVars)
                                 values@  ((Node value@"list"     _):moreVals) -> "(" ++ context ++ ", " ++ providedRequired ++ "(" ++ interface ++ ")" ++ ", " ++ parameter ++ ") ↦ " ++ (showList values)
                                 letters                                       -> "(" ++ context ++ ", " ++ providedRequired ++ "(" ++ interface ++ ")" ++ ", " ++ parameter ++ ") ↦ " ++ (fmap toLetter letters)
        showSub x = error $ "showSub " ++ (show x)
        
        showList items = "[" ++ (concat $ intersperse "," (fmap showItem items)) ++ "]"
          where showItem (Node "list" letters) = fmap toLetter letters
                showItem (Node item []) = item
        
        -- toLetter (Node "list" [Node letter []]) = chr (read letter :: Int)
        toLetter (Node letter [])               = chr (read letter :: Int)
        toLetter  x                             = error $ "toLetter " ++ (show x)
showWorld w x = error $ "showWorld " ++ (show w) ++ " " ++ (show x)

insertDescriptions :: [Term] -> Proof -> Proof
insertDescriptions descs proof = fmap f proof
  where f (World world)            = World world -- World ((>>=) world g)
        f (Assertion t)            = Assertion ((>>=) t g)
        f (AbbreviatedProof t n)   = AbbreviatedProof ((>>=) t g) n
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
  

worldParser :: Parser (World, [Proof])
worldParser = do
    reserved "WORLD"
    colon
    world <- term
    result <- brackets $ do assertion `sepBy` (reservedOp ",")
    reserved "length"
    colon
    integer
    return (world, result)

resultParser :: Parser AnalysisResult
resultParser = do
    reserved "justify6"
    colon
    many worldParser


simple :: Parser Proof
simple = do
  t <- term
  reservedOp "','" <|> void comma
  subproofs <- brackets $ assertion `sepBy` comma
  return $ Node (Assertion t) subproofs

notSimple :: Parser Proof
notSimple = do
  reserved "not"
  t <- term
  reservedOp "','" <|> void comma
  subproofs <- brackets $ (try notComplex <|> assertion) `sepBy` comma
  return $ Node (Not $ Assertion t) subproofs

notComplex :: Parser Proof
notComplex = parens $ do
  reserved "not"
  left <- parens $ term `sepBy` (reserved "','" <|> void comma)
  reservedOp "="
  right <- parens $ term `sepBy` (reserved "','" <|> void comma)
  reservedOp "','" <|> void comma
  brackets $ return ()
  return $ Node (NotEq left right) []

assertion :: Parser Proof
assertion =  parens $ try simple <|> notSimple


star :: Parser String
star = do
  reserved "*"
  return "*"

term :: Parser Term
term = try normal <|> (try list <|> tuple)
  where normal = do
                   f   <- identifier <|> (liftM show integer) <|> (char '\'' *> (many $ noneOf ['\'']) <* char '\'') <|> star
                   xs  <- option [] $ parens $ term `sepBy` comma
                   return $ Node f xs
        list   = do
                   terms <- brackets $ term `sepBy` comma
                   return $ Node "list" terms
        tuple  = do
                   terms <- parens $ term `sepBy` (reserved "','" <|> void comma)
                   return $ Node "tuple" terms

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
