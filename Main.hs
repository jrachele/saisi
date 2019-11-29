module Main where

-- Simple Interpreter by Julian Rachele
-- Special thanks to Alexey Kutepov
-- For his Haskell JSON parser from which my Tokenizer was inspired

import Control.Applicative
import Data.Char
import Data.Fixed

main :: IO ()
main = undefined


-- Tokenizer
-----------------------------------------------
data Token
    = Number Double
    | Identifier String
    | FnKeyword
    | Parenthesis Char
    | Op Operator
    deriving (Show, Eq)

data Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Assignment
    | Function
    deriving (Show, Eq)

newtype Tokenizer a = Tokenizer {
    tokenize :: String -> Maybe (a, String)
}

-- Functor allows us to modify the result of the tokenizer if valid
instance Functor Tokenizer where
    fmap f (Tokenizer t) = Tokenizer tok
        where tok input = do
                -- Run the tokenizer given to the functor on arbitrary input 'input'
                (result, rest) <- t input
                -- Return the normal result of the tokenizer with the function called on 'result'
                Just ((f result), rest)

-- Applicative allows Haskell to treat the tokenizer with respect
instance Applicative Tokenizer where
    pure a = Tokenizer (\i -> Just (a, i))
    (Tokenizer funcTokenizer) <*> (Tokenizer normTokenizer) = Tokenizer tok
        where tok input = do
                -- Run the first tokenizer on the input and extract its function
                (func, rest) <- funcTokenizer input
                -- Run the secon tokenizer to get a particular value
                (result, rest') <- normTokenizer rest
                -- Return the result of applying that function to the value
                Just (func result, rest')

-- Essentially lets us OR tokenizers to build them up
instance Alternative Tokenizer where
    empty = Tokenizer (const Nothing)
    Tokenizer t1 <|> Tokenizer t2 = Tokenizer tok
        -- We can take advantage of the fact that Maybe is also alternative
        -- and use the <|> operator on the results from t1 and t2
        where tok input = (t1 input) <|> (t2 input)


---- Helper tokenizers
-----------------------------------------------
-- Allows creating any arbitrary single character tokenizer
charTokenizer :: Char -> Tokenizer Char
charTokenizer c = Tokenizer firstChar
    where firstChar :: String -> Maybe (Char, String)
          firstChar (x:rest) = if c==x then Just (x, rest) else Nothing
          firstChar _ = Nothing

-- Allows creating any arbitrary string tokenizer (basically all of em)
stringTokenizer :: String -> Tokenizer String
stringTokenizer = sequenceA . map charTokenizer

-- Tokenizes a string based on a predicate
spanTokenizer :: (Char -> Bool) -> Tokenizer String
spanTokenizer f = Tokenizer tok
            where tok string = let (satisfiesPredicate, rest) = span f string in
                               Just (satisfiesPredicate, rest)

-- Whitespace tokenizer
ws :: Tokenizer String
ws = spanTokenizer isSpace

notEmpty :: Eq a => Tokenizer [a] -> Tokenizer [a]
notEmpty (Tokenizer t) = Tokenizer tok
    where tok input = do
            (a, rest) <- t input
            if a == [] then Nothing else Just (a, rest)
---------------------------------------------

parenthesis :: Tokenizer Token
parenthesis = fmap Parenthesis (charTokenizer '(' <|> charTokenizer ')')

number :: Tokenizer Token
number = fmap Number $ fmap read $ notEmpty $ spanTokenizer (\c -> c `elem` ['0'..'9'] ++ ['.'])

identifier :: Tokenizer Token
identifier = fmap Identifier $ notEmpty $ spanTokenizer (\c -> c `elem` (['A'..'Z']++['a'..'z']++['0'..'9']++['_']))

fnKeyword :: Tokenizer Token
fnKeyword = fmap (const FnKeyword) (stringTokenizer "fn")

fnOperator :: Tokenizer Token
fnOperator = fmap (const (Op Function)) (stringTokenizer "=>")

addOperator :: Tokenizer Token
addOperator = fmap (const (Op Add)) (charTokenizer '+')

subtractOperator :: Tokenizer Token
subtractOperator = fmap (const (Op Subtract)) (charTokenizer '-')

multiplyOperator :: Tokenizer Token
multiplyOperator = fmap (const (Op Multiply)) (charTokenizer '*')

divideOperator :: Tokenizer Token
divideOperator = fmap (const (Op Divide)) (charTokenizer '/')

moduloOperator :: Tokenizer Token
moduloOperator = fmap (const (Op Modulo)) (charTokenizer '%')

equalsOperator :: Tokenizer Token
equalsOperator = fmap (const (Op Assignment)) (charTokenizer '=')

-- Main tokenizer that aggregates all smaller parsers together
token :: Tokenizer Token
token = fnKeyword        <|> fnOperator         <|>     number          <|>
        identifier       <|> parenthesis        <|>     addOperator     <|>
        subtractOperator <|> multiplyOperator   <|>     divideOperator  <|>
        moduloOperator   <|> equalsOperator

tokenizer :: Tokenizer [Token]
tokenizer = ws *> ((:) <$> token <*> many (ws *> token) <|> pure [])

-- Parser
-----------------------------------------------
data Tree
    = NumLeaf Double
    | IdLeaf String
    | BinaryOp Operator Tree Tree
    | Assign String Tree
    | FunctionCall String Tree
    | Empty
    deriving (Show)


newtype Parser a = Parser {
    parse :: [Token] -> Maybe (a, [Token])
}

instance Functor Parser where
    fmap f (Parser p) = Parser b
      where b input = do
                (t, rest) <- p input
                Just (f t, rest)

instance Applicative Parser where
    pure x = Parser (\i -> Just (x, i))
    (Parser p1) <*> (Parser p2) = Parser b
      where b input = do
                (func, rest) <- p1 input
                (e, rest') <- p2 rest
                Just (func e, rest')

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p1) <|> (Parser p2) = Parser b
      where b input = do
                (p1 input) <|> (p2 input)

instance Monad Parser where
    (Parser p) >>= f = Parser b
      where b input = case p input of
                      Nothing -> Nothing
                      Just (x, r) -> parse (f x) r

-- GRAMMAR:
-- expr ::- term (+- expr)
-- term :: factor (*/ term)
-- factor :: (expr) | nat
-- nat :: Number | Identifier

expr :: Parser Tree
expr = do
       t <- term
       do o <- additive_operator
          e <- expr
          return (BinaryOp o t e)
          <|> return t

term :: Parser Tree
term = do
       f <- factor
       do o <- multiplicative_operator
          t <- term
          return (BinaryOp o f t)
          <|> return f

factor :: Parser Tree
factor = (paren '(' *> expr <* paren ')') <|> nat

nat :: Parser Tree
nat = Parser p
  where p ((Identifier i) : ts) = Just ((IdLeaf i), ts)
        p ((Number n) : ts) = Just ((NumLeaf n), ts)
        p _ = Nothing

additive_operator :: Parser Operator
additive_operator = Parser p
  where p ((Op o): ts) = case o of
            Add -> Just (o, ts)
            Subtract -> Just (o, ts)
            _ -> Nothing
        p _ = Nothing

multiplicative_operator :: Parser Operator
multiplicative_operator = Parser p
  where p ((Op o): ts) = case o of
            Multiply -> Just (o, ts)
            Divide -> Just (o, ts)
            Modulo -> Just (o, ts)
            _ -> Nothing
        p _ = Nothing

paren :: Char -> Parser Char
paren c = Parser p
  where p ((Parenthesis x) : ts) = if c==x then Just (x, ts) else Nothing
        p _ = Nothing

parseFromString :: String -> Maybe (Tree, [Token])
parseFromString s = do
                    (tokens, _) <- tokenize tokenizer s
                    parse expr tokens


eval :: Tree -> Result
eval (NumLeaf n) = Just n
eval (BinaryOp Add t1 t2) = (+) <$> eval t1 <*> eval t2
eval (BinaryOp Subtract t1 t2) = (-) <$> eval t1 <*> eval t2
eval (BinaryOp Multiply t1 t2) = (*) <$> eval t1 <*> eval t2
eval (BinaryOp Divide t1 t2) = (/) <$> eval t1 <*> eval t2
eval (BinaryOp Modulo t1 t2) = mod' <$> eval t1 <*> eval t2
eval _ = Nothing

type Result = Maybe Double

interpret :: String -> Result
interpret s = do
            (t, _) <- parseFromString s
            eval t

--newtype Interpreter a = Interpreter {
--  interpret :: Tree -> Result
--}
--
--newInterpreter :: Interpreter
--newInterpreter = undefined
--
--input :: String -> Interpreter -> Either String (Result, Interpreter)
--input _ _ = Left "Not implemented"