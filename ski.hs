import Data.Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Control.Monad.Combinators
import Data.Void

data Calculi = Variable String | String :=> Calculi | Calculi :$ Calculi deriving Show
infixr :=>

type Parser = Parsec Void String

term :: Parser Calculi
term = fmap (foldl1 (:$)) $ some $ named <|> parens

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
 (current :=>) <$> term


data Ski = S | K | Ski :! Ski deriving Show

pretty :: Ski -> String
pretty S = "s"
pretty K = "k"
pretty (function :! argument) = pretty function ++ "(" ++ pretty argument ++ ")"

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
expand (name :=> text) = ILambda name (expand text)
expand (function :$ argument) = ICall (expand function) (expand argument)

reduce :: Intermidate -> Maybe Ski
reduce (IVariable _) = Nothing
reduce (ICall function argument) = pure (:!) <*> reduce function <*> reduce argument
reduce (ILambda _ _) = Nothing
reduce IS = Just S
reduce IK = Just K
reduce II = Just $ (S :! K) :! K

convert :: Calculi -> Maybe Ski
convert = reduce . simplify . expand

main = do
 stdin <- getContents
 let lambda = runParser (space *> term) "stdin" stdin
 case lambda of
  Left error -> putStrLn $ errorBundlePretty error
  Right valid -> case convert valid of
   Nothing -> putStrLn "error: free variables"
   Just valid -> putStrLn $ pretty valid
