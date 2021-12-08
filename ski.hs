{-# LANGUAGE DeriveTraversable #-}

import Control.Monad.Combinators
import Data.List
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

data Format = Format
  { formatCall :: String,
    formatS :: String,
    formatK :: String
  }

pretty :: Format -> (Term Void Void) -> String
pretty (Format _ s _) S = s
pretty (Format _ _ k) K = k
pretty format@(Format call _ _) (Call function argument) = replace call
  where
    replace ('%' : '%' : xs) = '%' : replace xs
    replace ('%' : 'f' : xs) = pretty format function ++ replace xs
    replace ('%' : 'x' : xs) = pretty format argument ++ replace xs
    replace (c : xs) = c : replace xs
    replace [] = []
pretty _ (Variable x) = absurd x
pretty _ (Lambda x _) = absurd x

data CommandLine = CommandLine
  { inputFile :: String,
    outputFile :: String,
    format :: Format,
    help :: Bool
  }

commandDefault = CommandLine "-" "-" (Format "%f(%x)" "s" "k") False

parseFlags :: [String] -> IO CommandLine
parseFlags ("--help" : xs) = do
  command <- parseFlags xs
  pure $ command {help = True}
parseFlags ("--format" : call : s : k : xs) = do
  command <- parseFlags xs
  pure $ command {format = Format call s k}
parseFlags ("-o" : outputFile : xs) = do
  command <- parseFlags xs
  pure $ command {outputFile = outputFile}
parseFlags (flag : xs) | flag /= "-" && isPrefixOf "-" flag = do
  die $ "Unknown flag: " ++ flag
parseFlags (inputFile : xs) = do
  command <- parseFlags xs
  pure $ command {inputFile = inputFile}
parseFlags [] = pure commandDefault

main = do
  args <- getArgs
  command <- parseFlags args
  if help command
    then do
      putStrLn "Usage: ski [options] [inputfile]"
      putStrLn "Options:"
      putStrLn " --format format s k"
      putStrLn "     Specify a format for the ski output. Where `s` and `k` are strings that represent the combinator."
      putStrLn "     'format' specifies the application syntax, where '%f' is function and '%x' is argument."
      putStrLn "     For example: '%f(%x)' or 'a%f%x'"
      putStrLn " -o file"
      putStrLn "     Specify an output file. Standard Output is printed by default"
      exitSuccess
    else pure ()
  input <-
    if inputFile command == "-"
      then getContents
      else readFile (inputFile command)
  let lambda = runParser (white *> term Map.empty) (inputFile command) input
  case lambda of
    Left error -> die $ errorBundlePretty error
    Right valid ->
      if outputFile command == "-"
        then putStrLn $ pretty (format command) (simplify valid)
        else writeFile (outputFile command) $ pretty (format command) (simplify valid)
