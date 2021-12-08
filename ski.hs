{-# LANGUAGE DeriveTraversable #-}

import Control.Monad.Combinators
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void
import System.Environment (getArgs)
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char

-- https://www.staff.city.ac.uk/~ross/papers/debruijn.html

type Variables x = Map String x

type Parser = Parsec Void String

data Term l x = Variable x | Call (Term l x) (Term l x) | Lambda l (Term l (Maybe x)) | S | K deriving (Show, Functor, Foldable, Traversable)

comment :: Parser ()
comment = string "//" *> takeWhileP (Just "comment") (\x -> x /= '\n') *> pure ()

white :: Parser ()
white = space *> (comment *> white <|> pure ())

term :: Variables x -> Parser (Term () x)
term sym = fmap (foldl1 Call) $ some $ named sym <|> parens sym

letin :: Variables x -> String -> Parser (Term () x)
letin sym name = do
  string "=" *> white
  value <- term sym
  string ";" *> white
  text <- term (Map.singleton name Nothing <> Map.map Just sym)
  return $ Call (Lambda () text) value

lambda :: Variables x -> String -> Parser (Term () x)
lambda sym name = do
  string "=>"
  white
  Lambda () <$> term (Map.singleton name Nothing <> Map.map Just sym)

named :: Variables x -> Parser (Term () x)
named sym = do
  name <- some alphaNumChar <* white
  lambda sym name <|> letin sym name <|> case Map.lookup name sym of
    Nothing -> fail $ "free variable: " ++ name
    Just x -> pure (Variable x)

parens :: Variables x -> Parser (Term () x)
parens sym = between (string "(" <* white) (string ")" <* white) (term sym)

-- https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis

simplify :: Term () x -> Term l x
simplify (Variable x) = Variable x
simplify (Call e e') = Call (simplify e) (simplify e')
simplify (Lambda () e) = simplifyLambda e
simplify S = S
simplify K = K

simplifyLambda :: Term () (Maybe x) -> Term l x
simplifyLambda (Call e (Variable Nothing)) | Just e <- sequence e = simplify e
simplifyLambda (Variable Nothing) = (S `Call` K) `Call` K
simplifyLambda (Variable (Just x)) = K `Call` Variable x
simplifyLambda (Call e e') = case (sequence e, sequence e') of
  (Just e, Just e') -> K `Call` simplify e `Call` simplify e'
  _ -> S `Call` simplifyLambda e `Call` simplifyLambda e'
simplifyLambda (Lambda () e) = simplifyLambda $ simplifyLambda e
simplifyLambda S = K `Call` S
simplifyLambda K = K `Call` K

pretty :: String -> String -> String -> (Term Void Void) -> String
pretty _ s _ S = s
pretty _ _ k K = k
pretty format s k (Call function argument) = replace format
  where
    replace ('f' : xs) = pretty format s k function ++ replace xs
    replace ('x' : xs) = pretty format s k argument ++ replace xs
    replace (c : xs) = c : replace xs
    replace [] = []
pretty _ _ _ (Variable x) = absurd x
pretty _ _ _ (Lambda x _) = absurd x

main = do
  args <- getArgs
  let (format, s, k) = case args of
        [f] -> (f, "s", "k")
        [f, s, k] -> (f, s, k)
        _ -> ("f(x)", "s", "k")
  stdin <- getContents
  let lambda = runParser (white *> term Map.empty) "stdin" stdin
  case lambda of
    Left error -> die $ errorBundlePretty error
    Right valid -> putStrLn $ pretty format s k (simplify valid)
