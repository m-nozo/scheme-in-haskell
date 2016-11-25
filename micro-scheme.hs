import Data.Char

-- Lexer
data Token = Number Double
           | Symbol String
           | Lpar | Rpar | Quote | Dot
           | Other Char
           | Eof deriving Show

lexer :: String -> [Token]
lexer []      = [Eof]
lexer (x:xs)
  | isSpace x = lexer xs
  | isDigit x = let [(y, ys)] = reads (x:xs)
                       in Number y : lexer ys
  | isVar   x = let (y, ys)   = span isVar (x:xs)
                       in Symbol y : lexer ys
  | otherwise = case x of
                  '('  -> Lpar     : lexer xs
                  ')'  -> Rpar     : lexer xs
                  '\'' -> Quote    : lexer xs
                  '.'  -> Dot      : lexer xs
                  y    -> Other y  : lexer xs

isVar :: Char -> Bool
isVar c = isAlpha c || elem c "!$%&*+-/:<=>?@^_~"

-- Parser
data SExp = NUM Double
          | SYM String
          | CEL SExp SExp
          | NIL
          | FUN (Env -> SExp -> Evaluator SExp)
          | FEXP (Env -> SExp -> Evaluator SExp)

instance Show SExp where
  show val = case val of
    NUM x   -> show x
    SYM x   -> x
    CEL a d -> "(" ++ show a ++ showListElems d
    NIL     -> "()"
    FUN _   -> "<Function>"
    FEXP _  -> "<Fexpr>"

showListElems :: SExp -> String
showListElems (CEL a d) = " " ++ show a ++ showListElems d
showListElems NIL       = ")"
showListElems x         = " . " ++ show x ++ ")"

type Parser a = Either String a

parseErr :: String -> Parser a
parseErr s = Left s

parser :: [Token] -> Parser (SExp, [Token])
parser ts = case ts of
  (Eof      :xs) -> parseErr "Eof"
  (Number x :xs) -> return (NUM x, xs)
  (Symbol x :xs) -> return (SYM x, xs)
  (Lpar     :xs) -> readCell xs
  (Quote    :xs) -> do (y, ys) <- parser xs
                       return ((CEL (SYM "quote") (CEL y NIL)), ys)
  (x        :xs) -> parseErr $ "Unexpected Token: " ++ show x

readCell :: [Token] -> Parser (SExp, [Token])
readCell xs =
    case xs of
      (Rpar:t) -> return (NIL, t)
      (Dot :t) -> do (y, ys) <- parser t
                     return (y, tail ys)
      _        -> do (y, ys) <- parser xs
                     (z, zs) <- readCell ys
                     return (CEL y z, zs)

-- Eval
type Evaluator a = Either String a
type Env = [(String, SExp)]

evalErr :: String -> Evaluator a
evalErr s = Left s

eval :: Env -> SExp -> Evaluator SExp
eval env val = case val of
  NIL      -> return val
  NUM _    -> return val
  FUN _    -> return val
  CEL a d  -> do func <- eval env a
                 case func of
                   FEXP f -> f env d
                   FUN  f -> do args <- evalArgs env d
                                f env args
                   _      -> evalErr $ "Not Function:" ++ show func
  SYM name -> case lookup name env of
                Just y  -> return y
                Nothing -> evalErr $ "Undefinded:" ++ name

evalArgs :: Env -> SExp -> Evaluator SExp
evalArgs _   NIL       = return NIL
evalArgs env (CEL a d) = do y <- eval env a
                            z <- evalArgs env d
                            return (CEL y z)
evalArgs env x         = do y <- eval env x
                            return y

-- Function
slength :: SExp -> Int
slength (CEL _ d) = 1 + slength d
slength _         = 0

car :: SExp -> SExp
car (CEL x _) = x

cdr :: SExp -> SExp
cdr (CEL _ x) = x
                    
subr_car :: (Env -> SExp -> Evaluator SExp)
subr_car env x = if (slength x) /= 1 then
                   evalErr "Invalid arg"
                 else
                   case car x of
                     (CEL a _) -> return a
                     _         -> evalErr "Invalid cell"

subr_cdr :: (Env -> SExp -> Evaluator SExp)
subr_cdr env x = if (slength x) /= 1 then
                   evalErr "Invalid arg"
                 else
                   case car x of
                     (CEL _ d) -> return d
                     _         -> evalErr "Invalid cell"

quote :: Env -> SExp -> Evaluator SExp
quote env x = if (slength x) /= 1 then
                evalErr "Invalid arg"
              else
                return $ car x

lambda :: Env -> SExp -> Evaluator SExp
lambda env x = if (slength x) /= 2 then
                 evalErr "Invalid arg"
               else
                 let func _ args = do
                       env' <- createEnv env (car x) args
                       eval env' (car $ cdr x)
                 in return (FUN func)

createEnv :: Env -> SExp -> SExp -> Evaluator Env
createEnv env (SYM name) exp = return ((name, exp):env)
createEnv env NIL        _ = return env
createEnv env (CEL (SYM name) syms) (CEL exp exps) = do
  envs <- createEnv env syms exps
  return ((name, exp):envs)
createEnv _ _ _ = evalErr "createEnv error"

-- Main
initEnv :: Env
initEnv = [ ("car", FUN subr_car)
          , ("cdr", FUN subr_cdr)
          , ("quote", FEXP quote)
          , ("lambda", FEXP lambda)
          ]

run :: String -> IO ()
run xs =
    loop $ lexer xs where
    loop [Eof] = putStr ""
    loop xs    =
        case parser xs of
          Left  perr          -> print perr
          Right (exp, tokens) ->
            do case eval initEnv exp of
                 Left  eerr -> print eerr
                 Right ans  -> do print ans
                                  loop tokens
