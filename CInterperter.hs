module CInterperter (run,stmt,evalStmt) where
import Data.Char(isAlpha, isDigit, isSpace)
--import Debug.Trace
import Text.ParserCombinators.ReadP


{-
A statement is one of the following
  1. An if-else statement
     When we read it in, it is of the form:
       if (condition) statement else statement
  2. A while statement
     When we read it in, it is of the form:
       while (condition) statement
  3. An assignment statement
     When we read it in, it is of the form:
       variable = expression;
  4. A block of statements
     When we read it in, it is of the form:
       { statement statement ... statement }
     with zero or more statements in curly brackets
  5. A declaration of a variable
     When we read it in, it is of the form:
       int variable;
     so the only data type is integer
     A variable is initialized as zero when declared
     A variable is made up entirely of letters
-}
data Statement = IfElse Condition Statement Statement |
                 While Condition Statement |
                 Assign Expression Expression |
                 Block [Statement] |
                 Declare Expression
  deriving Show

{-
A condition is read in as one of the following forms:
  1. expression < expression
  2. expression > expression
  3. expression <= expression
  4. expression >= expression
  5. expression == expression
  6. expression != expression
  7. condition && condition
  8. condition || condition
  9. ! condition
Note:  Comparison operators have th highest precedence
  followed by "!", followed by "&&" followed by "||"
-}
data Condition = Less Expression Expression |
                 Greater Expression Expression |
                 LessEq Expression Expression |
                 GreaterEq Expression Expression |
                 Equal Expression Expression |
                 NotEqual Expression Expression |
                 EqCond Condition Condition |
                 NotEqCond Condition Condition |
                 And Condition Condition |
                 Or Condition Condition |
                 Not Condition
  deriving Show

{-
An expression is read in as one of the folowing forms:
  1. expression + expression
  2. expression - expression
  3. expression * expression
  4. expression / expression
  5. variable
  6. number
Note:  "*" and "/" have precedence over "+" and "-"
-}
data Expression = Plus Expression Expression |
                   Minus Expression Expression |
                   Times Expression Expression |
                   Divide Expression Expression |
                   Var String |
                   Num Int |
                   Str String 
  deriving Show

{-
Memory is a set of pairs consisting of
  - a variable
  - the current value of that variable
Variables could be duplicated in memory
  then I will assume the first occurrence
  of a variable gives the current value
-}
type Memory = [(String,Int)]

{-
This function will parse your input and run the program
A program is a list of statements surrounded by curly brackets
  in other words, a program is a statement
When you run your program, initially the memory is empty
This function will return the memory when the program is completed
-}
run :: String -> Memory
--
run input = evalStmt (parse stmt input) []

{-
To evaluate a statement you give
  1. the statement
  2. the current memory
It returns the memory after the statement is executed
-}
evalStmt :: Statement -> Memory -> Memory 
--evalStmt stmt mem | trace ("evalStmt \n" ++ show stmt ++ "  " ++ show mem) False = undefined
--
evalStmt (IfElse cond stmt1 stmt2) currMem
   | evalCond cond currMem = evalStmt stmt1 currMem
   | otherwise = evalStmt stmt2 currMem
evalStmt entire@(While cond stmt) currMem
   | evalCond cond currMem = evalStmt entire (evalStmt stmt currMem)
   | otherwise  =  currMem
evalStmt (Assign (Var var) exp) currMem = (var, evalExp exp currMem):currMem
evalStmt (Block []) currMem = currMem
evalStmt (Block (stmt:stmts)) currMem = evalStmt (Block stmts) $ evalStmt stmt currMem
evalStmt (Declare (Var var)) currMem = (var, 0):currMem

{-
To evaluate a condition you give
  1. the condition
  2. the current memory
It returns a bool indicating if the condition is true
-}
evalCond :: Condition -> Memory -> Bool
--
evalCond (Less exp1 exp2) currMem = evalExp exp1 currMem < evalExp exp2 currMem
evalCond (Greater exp1 exp2) currMem = evalExp exp1 currMem > evalExp exp2 currMem
evalCond (LessEq exp1 exp2) currMem = evalExp exp1 currMem <= evalExp exp2 currMem
evalCond (GreaterEq exp1 exp2) currMem = evalExp exp1 currMem >= evalExp exp2 currMem
evalCond (Equal exp1 exp2) currMem = evalExp exp1 currMem == evalExp exp2 currMem
evalCond (NotEqual exp1 exp2) currMem = evalExp exp1 currMem /= evalExp exp2 currMem
evalCond (And cond1 cond2) currMem = evalCond cond1 currMem && evalCond cond2 currMem
evalCond (Or cond1 cond2) currMem = evalCond cond1 currMem || evalCond cond2 currMem
evalCond (Not cond) currMem = not(evalCond cond currMem)
evalCond (EqCond cond1 cond2) currMem = evalCond cond1 currMem == evalCond cond2 currMem
evalCond (NotEqCond cond1 cond2) currMem = evalCond cond1 currMem /= evalCond cond2 currMem

{-
To evaluate an expression you give
  1. the expression
  2. the current memory
It returns the value of the expression
-}
evalExp :: Expression -> Memory -> Int
--
evalExp (Plus exp1 exp2) currMem = evalExp exp1 currMem + evalExp exp2 currMem
evalExp (Minus exp1 exp2) currMem = evalExp exp1 currMem - evalExp exp2 currMem
evalExp (Times exp1 exp2) currMem = evalExp exp1 currMem * evalExp exp2 currMem
evalExp (Divide exp1 exp2) currMem = evalExp exp1 currMem `div` evalExp exp2 currMem
evalExp (Var var) currMem = snd $ head $ filter (\x -> fst x == var) currMem
evalExp (Num num) currMem = num


-- This parses a statement and stores the result
stmt :: ReadP Statement
--
stmt = choice [blockStmt, ifStmt, whileStmt, declareStmt, assignStmt]

ifStmt :: ReadP Statement
--
ifStmt = do string "if"
            c <- between (char '(') (char ')') cond
            s1 <- stmt
            string "else"
            IfElse c s1 <$> stmt

whileStmt :: ReadP Statement
--
whileStmt = do string "while"
               c <- between (char '(') (char ')') cond
               While c <$> stmt

assignStmt :: ReadP Statement
--
assignStmt = do var <- varExpr
                exp <- between (char '=') (char ';') expr
                return $ Assign var exp

blockStmt :: ReadP Statement
--
blockStmt =  do stmts <- between (char '{') (char '}') $ many stmt
                return $ Block stmts

declareStmt :: ReadP Statement
--
declareStmt = do var <- between (string "int") (char ';') expr
                 return $ Declare var

-- This parses an expression and stores the result
expr :: ReadP Expression
--
expr = chainl1 chainedTimesExpr plus

plus :: ReadP (Expression -> Expression -> Expression)
--
plus = do op <- char '+' <++ char '-'
          return $ makeExpr op

chainedTimesExpr:: ReadP Expression
--
chainedTimesExpr = chainl1 baseExpr times

times = do op <- char '*' <++ char '/'
           return $ makeExpr op

makeExpr :: Char -> Expression -> Expression -> Expression
--
makeExpr '+' e1 e2 = Plus e1 e2
makeExpr '-' e1 e2 = Minus e1 e2
makeExpr '*' e1 e2 = Times e1 e2
makeExpr '/' e1 e2 = Divide e1 e2           

baseExpr :: ReadP Expression
--
baseExpr = choice [numExpr, varExpr]

varExpr :: ReadP Expression
--
varExpr = do e <- munch1 isAlpha
             return $ Var e

numExpr :: ReadP Expression
--
numExpr = do e <- munch1 isDigit
             return $ Num (read e::Int)


-- This parses a condition and stores the result
cond :: ReadP Condition
--
cond = chainl1 chainedAndCond orCond
            
chainedAndCond = chainl1 chainedEqCond andCond

andCond :: ReadP (Condition -> Condition -> Condition)
--
andCond = do string "&&"
             return And

orCond :: ReadP (Condition -> Condition -> Condition)
--
orCond = do string "||"
            return Or

chainedEqCond = chainl1 baseCond eqCond

baseCond :: ReadP Condition
--
baseCond = choice [compCond, notCond]

makeEqCond :: String -> Condition -> Condition-> Condition
--
makeEqCond "==" = EqCond
makeEqCond "!=" = NotEqCond

eqCond :: ReadP (Condition -> Condition -> Condition)
--
eqCond = do op <- choice [string "==", string "!="]
            return $ makeEqCond op

compCond :: ReadP Condition
--
compCond = do opening <- many (char '(')
              e1 <- expr
              op <- choice [string "<", string ">", string "<=", string ">=", string "==", string "!="]
              e2 <- expr
              string [')' | char <- opening]
              return $ makeCond op e1 e2
              
makeCond :: String -> Expression -> Expression -> Condition
makeCond "<" e1 e2 = Less e1 e2
makeCond ">" e1 e2 = Greater e1 e2
makeCond "<=" e1 e2 = LessEq e1 e2
makeCond ">=" e1 e2 = GreaterEq e1 e2
makeCond "==" e1 e2 = Equal e1 e2
makeCond "!=" e1 e2 = NotEqual e1 e2

notCond :: ReadP Condition
--
notCond = do char '!'
             opening <- many (char '(')
             c <- compCond
             string [')' | char <- opening]
             return $ Not c

-- Run parser
parse_ :: ReadP a -> String -> a
--
parse_ p s
  | null parses = error "There are no parses"
  | length parses > 1 = error "There is more than one parse"
  | otherwise = head parses
    where parses = map fst (filter (null . snd) (readP_to_S p s))

parse :: ReadP a -> String -> a
--
parse p s = parse_ p (filter (not . isSpace ) s)
