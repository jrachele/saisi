module Main where

-- Simple Interpreter by Julian Rachele
-- Special thanks to Alexey Kutepov
-- For his Haskell JSON parser from which my Tokenizer was inspired

import Control.Applicative
import Data.Char

main :: IO ()
main = undefined

data Interpreter
type Result = Maybe Double

-- Create the grammar
data Token
    = Number Double
    | Identifier String
    | Operator
    | FnKeyword
    | FnOperator
    | Parenthesis Char
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equals
    deriving (Show, Eq)

-- Composite tokens that represent more complex syntax
-- data Composites
--     = FunctionCall Identifier Expression
--     | Assignment Identifier Expression
--     | Expression


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



-- Helper tokenizers
---------------------------------------------
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

notEmpty :: Tokenizer String -> Tokenizer String
notEmpty (Tokenizer t) = Tokenizer tok
    where tok input = do
            (a, rest) <- t input
            if a == "" then Nothing else Just (a, rest)
---------------------------------------------

number :: Tokenizer Token
number = fmap Number $ fmap read $ notEmpty $ spanTokenizer (\c -> c `elem` ['0'..'9'] ++ ['.'])

identifier :: Tokenizer Token
identifier = fmap Identifier $ notEmpty $ spanTokenizer (\c -> c `elem` (['A'..'Z']++['a'..'z']++['0'..'9']++['_']))

fnKeyword :: Tokenizer Token
fnKeyword = fmap (const FnKeyword) (stringTokenizer "fn")

fnOperator :: Tokenizer Token
fnOperator = fmap (const FnOperator) (stringTokenizer "=>")

parenthesis :: Tokenizer Token
parenthesis = fmap Parenthesis (charTokenizer '(' <|> charTokenizer ')')

addOperator :: Tokenizer Token
addOperator = fmap (const Add) (charTokenizer '+')

subtractOperator :: Tokenizer Token
subtractOperator = fmap (const Subtract) (charTokenizer '-')

multiplyOperator :: Tokenizer Token
multiplyOperator = fmap (const Multiply) (charTokenizer '*')

divideOperator :: Tokenizer Token
divideOperator = fmap (const Divide) (charTokenizer '/')

moduloOperator :: Tokenizer Token
moduloOperator = fmap (const Modulo) (charTokenizer '%')

equalsOperator :: Tokenizer Token
equalsOperator = fmap (const Equals) (charTokenizer '=')

-- Main tokenizer that aggregates all smaller parsers together
token :: Tokenizer Token
token = fnKeyword        <|> fnOperator         <|>     number          <|> 
        identifier       <|> parenthesis        <|>     addOperator     <|> 
        subtractOperator <|> multiplyOperator   <|>     divideOperator  <|> 
        moduloOperator   <|> equalsOperator

tokenizer :: Tokenizer [Token]
tokenizer = (:) <$> token <*> many (ws *> token) <|> pure []

newInterpreter :: Interpreter
newInterpreter = undefined

input :: String -> Interpreter -> Either String (Result, Interpreter)
input _ _ = Left "Not implemented"