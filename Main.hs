module Main where

    -- Simple Interpreter by Julian Rachele
    -- Special thanks to Alexey Kutepov
    -- For his Haskell JSON parser from which my Tokenizer was inspired
    
    import Control.Applicative
    import Data.Char
    import Data.Fixed
    import Data.Either
    import qualified Data.Map as M
    import Data.List
    import System.IO
    
    
    -- Tokenizer
    -----------------------------------------------
    data Token
        = Number Double
        | Identifier String
        | FnKeyword
        | Parenthesis Char
        | Op Operator
        deriving (Show, Eq, Ord)
    
    data Operator
        = Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | Assignment
        | Function
        deriving (Show, Eq, Ord)
    
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
    tokenizer = ws *> (some $ ws *> token) <* ws
    
    -- Parser
    -----------------------------------------------
    data Tree
        = NumLeaf Double
        | IdLeaf String
        | BinaryOp Operator Tree Tree
        | Assign String Tree
        | FunctionCall String [Tree]
        | FunctionDef String [String] Tree
        | Empty
        deriving (Show, Ord, Eq)
    
    
    -- Our parser is context dependent, solely to analyze function calls
    newtype Parser a = Parser {
        parse :: [Token] -> Interpreter -> Maybe (a, [Token])
    }
    
    instance Functor Parser where
        fmap f (Parser p) = Parser b
          where b input interpreter = do
                    (t, rest) <- p input interpreter
                    Just (f t, rest)
    
    instance Applicative Parser where
        pure x = Parser (\i _ -> Just (x, i))
        (Parser p1) <*> (Parser p2) = Parser b
          where b input interpreter = do
                    (func, rest) <- p1 input interpreter
                    (e, rest') <- p2 rest interpreter
                    Just (func e, rest')
    
    instance Alternative Parser where
        empty = Parser (\_ _ -> Nothing)
        (Parser p1) <|> (Parser p2) = Parser b
          where b input interpreter= do
                    (p1 input interpreter) <|> (p2 input interpreter)
    
    instance Monad Parser where
        (Parser p) >>= f = Parser b
          where b input interpreter = case p input interpreter of
                          Nothing -> Nothing
                          Just (x, r) -> parse (f x) r interpreter
    
    -- GRAMMAR:
    -- expr ::- term (+- expr)
    -- term :: factor (*/ term)
    -- factor :: (expr) | assignment | num | iden
    -- assignment :: identifier = expr
    -- num :: Number
    -- iden :: Identifier
    

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
    -- factor = num <|> iden <|> assignment <|> (paren '(' *> expr <* paren ')') <|> assignment <|> def <|> call 
    factor = (paren '(' *> expr <* paren ')') <|> assignment <|> def <|> call <|> iden <|> num 
    
    assignment :: Parser Tree
    assignment = do
                 i <- iden
                 do assignment_operator
                    e <- expr
                    let (IdLeaf identifier) = i in
                      return (Assign identifier e)
    
    additive_operator :: Parser Operator
    additive_operator = Parser p
      where p ((Op o): ts) _ = case o of
                Add -> Just (o, ts)
                Subtract -> Just (o, ts)
                _ -> Nothing
            p _ _ = Nothing
    
    multiplicative_operator :: Parser Operator
    multiplicative_operator = Parser p
      where p ((Op o): ts) _ = case o of
                Multiply -> Just (o, ts)
                Divide -> Just (o, ts)
                Modulo -> Just (o, ts)
                _ -> Nothing
            p _ _ = Nothing
    
    assignment_operator :: Parser Operator
    assignment_operator = Parser p
      where p ((Op Assignment): ts) _ = Just (Assignment, ts)
            p _ _ = Nothing
    
    -- fn name [args] = expr
    def :: Parser Tree
    def = do
          function_keyword
          fn_name <- iden
          args <- many iden
          function_operator
          body <- expr
          -- We need to unwrap the strings from fn_name and args
          let fn_name_str = unwrap_id fn_name in
            let args_str = unwrap_id <$> args in
              return (FunctionDef fn_name_str args_str body)
    
    
    -- constructs explicit function call when non singleton function is called
    call :: Parser Tree
    call = Parser p
      where
        p tokens interpreter = parse fn_call tokens interpreter
          where
            fn_call = do
              fn_name <- fn_iden
              case num_args (unwrap_id fn_name) interpreter of
                Just argc -> do
                  args <- sequenceA $ replicate argc expr
                  let fn_name_str = unwrap_id fn_name
                   in return (FunctionCall fn_name_str args)

    num :: Parser Tree
    num = Parser p
      where p ((Number n) : ts) _ = Just ((NumLeaf n), ts)
            p _ _ = Nothing
    
    iden :: Parser Tree
    iden = Parser p
      where p ((Identifier i) : ts) _ = Just ((IdLeaf i), ts)
            p _ _ = Nothing
    
    fn_iden :: Parser Tree
    fn_iden = Parser p
      where p ((Identifier i) : ts) interpreter = case (M.lookup i interpreter) of
                                                  Just (_, Func, _) -> Just ((IdLeaf i), ts)
                                                  _ -> Nothing
            p _ _ = Nothing


    -- the parser type doesn't matter; it only exists as a small piece in a larger pattern
    function_keyword :: Parser String
    function_keyword = Parser p
      where p (FnKeyword : ts) _ = Just ("fn", ts)
            p _ _ = Nothing
    
    function_operator :: Parser String
    function_operator = Parser p
      where p ((Op Function) : ts) _ = Just ("=>", ts)
            p _ _ = Nothing
    
    unwrap_id :: Tree -> String
    unwrap_id (IdLeaf i) = i
    unwrap_id _ = ""

    wrap_id :: String -> Tree
    wrap_id i = (IdLeaf i)

    -- validate_fn_tree :: Tree -> Tree -> Bool
    -- validate_fn_tree node (BinaryOp _ t1 t2) = (validate_fn_tree node t1) || (validate_fn_tree node t2)
    -- validate_fn_tree node (Assign _ tree) = validate_fn_tree node tree
    -- validate_fn_tree node tree = node == tree
    
    num_args :: String -> Interpreter -> Maybe Int
    num_args i interpreter =
      case M.lookup i interpreter of
        Just (args, Func, _) -> Just $ length args
        _                    -> Nothing

    paren :: Char -> Parser Char
    paren c = Parser p
      where p ((Parenthesis x) : ts) _ = if c==x then Just (x, ts) else Nothing
            p _ _ = Nothing
    
    parseFromString :: String -> Interpreter -> Maybe (Tree, [Token])
    parseFromString s interpreter = do
                        (tokens, []) <- tokenize tokenizer s
                        parse expr tokens interpreter
    
    -- Interpreter
    -------------------------------------
    
    -- M = Data.Map
    -- Interpreter maps ID -> ([ARGS], CONTENT), where ARGS is optional and only used for functions
    type Interpreter = M.Map String ([String], VarType, Tree)
    data VarType = Val | Func deriving (Show)
    
    type Result = Maybe Double
    
    newInterpreter :: Interpreter
    newInterpreter = M.empty
    
    -- Just as with the lexer and parsers above, the evaluator will take advantage of
    -- Applicatives to allow for easy chaining and composition
    data Evaluator a = Evaluator {
      evaluate :: Tree -> Interpreter -> Either String (Maybe a, Interpreter)
    }
    --
    instance Functor Evaluator where
      fmap f (Evaluator e) = Evaluator ev
        where ev tree state = case e tree state of
                          Left err -> Left err
                          Right (res, state') -> Right (f <$> res, state')
    
    instance Applicative Evaluator where
      pure x = Evaluator (\tree interpreter -> Right (Just x, interpreter))
      (Evaluator e1) <*> (Evaluator e2) = Evaluator e
        where e tree state = case e1 tree state of
                          Left err -> Left err
                          Right (f, state') -> case e2 tree state' of
                                                Left err -> Left err
                                                Right (x, state'') -> Right (f <*> x, state'')
    --
    instance Alternative Evaluator where
      empty = Evaluator (\_ i -> Right (Nothing, i))
      (Evaluator e1) <|> (Evaluator e2) = Evaluator e
        where e tree state = case e1 tree state of
                          Left err -> Left err
                          Right (f, state') -> case e2 tree state' of
                                                Left err -> Left err
                                                Right (x, state'') -> Right (f <|> x, state'')
    
    instance Monad Evaluator where
        (Evaluator e) >>= f = Evaluator ev
          where ev tree state = case e tree state of
                          Left err -> Left err
                          Right (Nothing, state') -> Right (Nothing, state')
                          Right (Just res, state') -> evaluate (f res) tree state'
    
    evalNum :: Evaluator Double
    evalNum = Evaluator e
      where e (NumLeaf n) s = Right (Just n, s)
            e _ s = Right (Nothing, s)
    
    evalIden :: Evaluator Double
    evalIden = Evaluator e
    -- TODO add types to global state table
      where e (IdLeaf i) state = case M.lookup i state of
                           -- If the identifier is valid then recursively evaluate the tree associated with it
                           Just ([], Val, tree) -> evaluate eval tree state
                           -- Singleton function call
                           Just (_, Func, tree) -> evaluate evalFunction (FunctionCall i []) state
                          --  Just (_, tree) -> Left $ "ERROR: Too few arguments for function call: '" ++ i ++ "'"
                           _ -> Left $ "ERROR: Unknown identifier: '" ++ i ++ "'"
            e _ s = Right (Nothing, s)
    
    evalAssignment :: Evaluator Double
    evalAssignment = Evaluator e
      where e (Assign identifier content) state = case (evaluate eval content state) of
                                -- If the content is invalid, the state is unchanged and the user is notified
                                Left err -> Left err
                                -- Otherwise, it's possible the content itself modified the state, which we should
                                -- take into account, then modify the state with the content's tree
                                Right (res, state) -> case M.lookup identifier state of
                                      -- you cannot declare a variable with the same name as a function
                                      Just (_, Func, _) -> Left $ "ERROR: Function already defined for identifier: '" ++ identifier ++ "'"
                                      _ -> Right (res, (M.insert identifier ([], Val, content) state))
            e _ s = Right (Nothing, s)
    
    evalBinaryOp :: Evaluator Double
    evalBinaryOp = Evaluator e
      where e (BinaryOp op t1 t2) state = do
                            (t1e, state') <- evaluate eval t1 state
                            (t2e, state'') <- evaluate eval t2 state
                            case op of
                              Add -> Right ((+) <$> t1e <*> t2e, state'')
                              Subtract -> Right ((-) <$> t1e <*> t2e, state'')
                              Multiply -> Right ((*) <$> t1e <*> t2e, state'')
                              Divide -> Right ((/) <$> t1e <*> t2e, state'')
                              Modulo -> Right (mod' <$> t1e <*> t2e, state'')
                              -- shouldn't ever happen though
                              _ -> Right (Nothing, state'')
    
            e _ s = Right (Nothing, s)
    
    evalFunctionDef :: Evaluator Double
    evalFunctionDef = Evaluator e
      where e (FunctionDef fn_name args body) state =
              -- firstly, we have to check that the function is not shadowing a variable
              case (M.lookup fn_name state) of
                Just (_, Val, _) -> Left $ "ERROR: Identifier already declared: '" ++ fn_name ++ "'"
                _ -> 
                    -- run the function to see if it returns an error or not
                    let run_args = replicate (length args) (NumLeaf 0) in 
                      case (evaluate evalFunction (FunctionCall fn_name run_args) (M.insert fn_name (args, Func, body) state)) of
                          Left err -> Left err
                          Right _ -> Right (Nothing, M.insert fn_name (args, Func, body) state)
            e _ s = Right (Nothing, s)
    
    evalFunction :: Evaluator Double
    evalFunction = Evaluator e
      where e (FunctionCall fn_name args) state =
              -- Use the variable state to evaluate the arguments, then
              -- replace all instances of arguments in the func state with the evaluated args, and
              -- evaluate normally
              case (M.lookup fn_name state) of
                Nothing -> Left $ "ERROR: Undefined function: '" ++ fn_name ++ "'"
                Just (identifiers, _, tree) ->
                                        if length args /= length identifiers
                                        then Left $ "ERROR: Mismatched number of arguments: ARGS: " ++ show args ++ "; IDENTIFIERS: " ++ show identifiers
                                        else
                                          -- Evaluate each argument under the current state
                                          let evaluated_args = fmap (\t -> evaluate eval t state) args in
                                            case find isLeft evaluated_args of
                                              -- make sure there were no problems with evaluation
                                              Just err -> err
                                              -- get the pure results from the argument list after evaluation
                                              Nothing -> let validated_args = (\(Right (Just x, _)) -> NumLeaf x) <$> evaluated_args in
                                                  let funcState = foldr (\(a, b) acc -> M.insert a ([], Val, b) acc) M.empty (zip identifiers validated_args) in
                                                    case evaluate eval tree funcState of
                                                      -- Preserve the state since the function will not modify global state
                                                      Right (res, _) -> Right (res, state)
                                                      Left err -> Left err
    
    
    
            e _ s = Right (Nothing, s)
    
    
    eval :: Evaluator Double
    eval = evalNum <|> evalFunction <|> evalIden <|> evalAssignment <|> evalBinaryOp <|> evalFunctionDef
    
    input :: String -> Interpreter -> Either String (Result, Interpreter)
    input string interpreter = case (all isSpace string) of
        True -> Right (Nothing, interpreter)
        False -> 
                case parseTree of
                    Just (tree, []) -> evaluate eval tree interpreter
                    _ -> Left "ERROR: Invalid syntax"
                where parseTree = parseFromString string interpreter
      
    
    main :: IO ()
    main = do
           -- Initialize with empty state
           let interpreter = newInterpreter
           loop interpreter
    
    prompt :: IO String
    prompt = do
           putStr "> "
           hFlush stdout
           getLine
    
    loop :: Interpreter -> IO ()
    loop currentState = do
           line <- prompt
           case (input line currentState) of
              Right (Just d, newState) -> do
    --                         putStrLn $ "DEBUG: " ++ show d ++ " ; STATE: " ++ show newState
                             putStrLn $ show d
                             loop newState
              Right (Nothing, newState) -> do
    --                         putStrLn $ "STATE: " ++ show newState
                             loop newState
              Left err -> do
                          putStrLn err
                          loop currentState