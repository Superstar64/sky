import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Control.Monad.Combinators

data Calculi = Variable String | Lambda String Calculi | Call Calculi Calculi deriving Show

type Parser = Parsec Void String

term :: Parser Calculi
term = fmap (foldl1 Call) $ some $ named <|> parens

named :: Parser Calculi
named = do
 name <- some alphaNumChar <* space
 lambda name <|> return (Variable name)

parens :: Parser Calculi
parens = between (string "(" <* space) (string ")" <* space) term

lambda :: String -> Parser Calculi
lambda current = do
 string "=>"
 space
 Lambda current <$> term


free :: Calculi -> Set String
free (Variable name) = Set.singleton name
free (Call function argument) = Set.union (free function) (free argument)
free (Lambda name text) = free text Set.\\ Set.singleton name

data Ski = S | K | SKCall Ski Ski deriving Show

pretty :: Ski -> String
pretty S = "s"
pretty K = "k"
pretty (SKCall function argument) = pretty function ++ "(" ++ pretty argument ++ ")"

-- https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis

data Intermidate = IVariable String | ICall Intermidate Intermidate | ILambda String Intermidate | IS | IK | II deriving Show

search :: String -> Intermidate -> Bool
search name (IVariable x) = name == x
search name (ILambda _ text) = search name text
search name (ICall left right) = search name left || search name right
search _ IS = False
search _ IK = False
search _ II = False

simplify :: Intermidate -> Intermidate
simplify (IVariable name) = IVariable name
simplify (ICall function argument) = ICall (simplify function) (simplify argument)
simplify (ILambda name term) | (not $ search name term) = ICall IK $ simplify term
simplify (ILambda name (IVariable name')) | name == name' = II
simplify (ILambda name (ILambda name' text)) = simplify $ ILambda name $ simplify $ ILambda name' text
simplify (ILambda name (ICall function argument)) = ICall (ICall IS $ simplify $ ILambda name function) $ simplify $ ILambda name argument
simplify IS = IS
simplify IK = IK
simplify II = II

expand :: Calculi -> Intermidate
expand (Variable name) = IVariable name
expand (Lambda name text) = ILambda name (expand text)
expand (Call function argument) = ICall (expand function) (expand argument)

reduce :: Intermidate -> Maybe Ski
reduce (IVariable _) = Nothing
reduce (ICall function argument) = pure SKCall <*> reduce function <*> reduce argument
reduce (ILambda _ _) = Nothing
reduce IS = Just S
reduce IK = Just K
reduce II = Just $ SKCall (SKCall S K) K

convert :: Calculi -> Maybe Ski
convert = reduce . simplify . expand

main = do
 stdin <- getContents
 let lambda = runParser (space *> term) "stdin" stdin
 case lambda of
  Left error -> putStrLn $ errorBundlePretty error
  Right valid -> case convert valid of
   Nothing -> do 
    putStrLn "error: free variables"
    print $ Set.toAscList $ free valid
   Just valid -> putStrLn $ pretty valid
