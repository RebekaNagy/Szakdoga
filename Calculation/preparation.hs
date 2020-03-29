module Preparation where
import Data.String
import Data.List
import Parser
import Verification

------------------------------------- MAIN CONVERSION FUNCTIONS

mainConversion :: [Statement] -> ((Environment, Environment), ([Program], [Program], [Program]))-> ((Environment, Environment), [Program])
mainConversion []  ((init, final), (actions, tables, prog)) = ((init, final), prog)
mainConversion (x:xs) ((init, final), (actions, tables, prog)) =
    case x of 
        ParserHeader name fields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
        ParserStruct sname sfields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
        Parser block -> mainConversion xs (((parserConversion x init), final), (actions, tables, prog))
        Control cname cblock -> mainConversion xs (controlConversion cblock ((init, final), (actions, tables, prog)))
        _ -> mainConversion xs ((init, final), (actions, tables, prog))

------------------------------------- HEADER CONVERSION FUNCTIONS

headerConversion :: Statement -> (Environment, Environment) -> (Environment, Environment)
headerConversion (ParserStruct name []) (Env init, Env final) = (Env init, Env final)
headerConversion (ParserHeader name []) (Env init, Env final) = (Env init, Env final)
headerConversion (ParserHeader name fields) (Env init, Env final) = (Env (init ++ [newHeader]), Env (final ++ [newHeader]))
    where newHeader = (name, (Invalid, fieldsConversion fields))
headerConversion (ParserStruct name ((StructField (structType, sname)):xs)) (Env init, Env final) 
    | isItHeader structType (Env init) = rewriteHeader ((StructField (structType, sname)):xs) (Env init, Env final)
    | otherwise = (Env (init ++ [newHeader]), Env (final ++ [newHeader]))
    where newHeader = (name, (Invalid, fieldsConversion ((StructField (structType, sname)):xs)))

fieldsConversion :: [Variable] -> [Field]
fieldsConversion [] = []
fieldsConversion ((ParserField (ptype, name)):xs) = (name, Valid) : fieldsConversion xs
fieldsConversion ((StructField (stype, name)):xs) = (name, Valid) : fieldsConversion xs

isItHeader :: String -> Environment -> Bool
isItHeader id (Env l) = or (map (\x@(id', (v', f)) -> if id == id' then True else False) l)

rewriteHeader :: [Variable] -> (Environment, Environment) -> (Environment, Environment)
rewriteHeader [] (Env init, Env final) = (Env init, Env final)
rewriteHeader ((StructField (stype, sname)):xs) (Env init, Env final) = rewriteHeader xs (Env newEnv, Env newEnv)
    where newEnv = (map (\x@(hname, (v, f)) -> if stype == hname then (sname, (v, (rewriteFields sname f))) else x) init)

rewriteFields :: String -> [Field] -> [Field]
rewriteFields headerName fields = map (\(name, v) -> ((headerName ++ "." ++ name), v)) fields

------------------------------------- PARSER CONVERSION FUNCTIONS
parserConversion :: Statement -> Environment -> Environment
parserConversion (Parser []) init = init
parserConversion (Parser (x:xs)) init =
    case x of 
        State name block -> parserConversion (Parser xs) (stateConversion block init)
        _ -> parserConversion (Parser xs) init

stateConversion :: Statement -> Environment -> Environment
stateConversion (ParserSeq []) init = init 
stateConversion (ParserSeq (x:xs)) init =
    case x of 
        FuncExpr (Extract (FuncVar pack) (FuncVar header)) -> 
            stateConversion (ParserSeq xs) (prFunc_SetHeaderValidity init (drop 1 (dropWhile (/= '.') header)) Valid)
        _ -> stateConversion (ParserSeq xs) init

------------------------------------- CONTROL CONVERSION FUNCTION
controlConversion :: [Statement] -> ((Environment, Environment), ([Program], [Program], [Program])) -> ((Environment, Environment), ([Program], [Program], [Program]))
controlConversion [] ((init, final),(actions, tables, prog)) = ((init, final),(actions, tables, prog))
controlConversion (x:xs) ((init, final),(actions, tables, prog)) =
    case x of 
        ParserAction actionName (ParserSeq acts) -> 
            controlConversion xs ((init, final), ((actions ++ [ActCons actionName (actionConversion acts)]), tables, prog))
        ParserTable tableName (Keys k) (Acts act) -> controlConversion xs ((init, final), (tableConversion x (actions, tables, prog)))
        Apply (ParserSeq block) -> controlConversion xs ((init, (emitConversion block final)), (actions, tables, (prog ++ [applyConversion block actions tables])))
        _ -> controlConversion xs ((init, final), (actions, tables, prog))

------------------------------------- ACTION CONVERSION FUNCTIONS
actionConversion :: [Statement] -> Program
actionConversion [] = Skip
actionConversion (x:xs) = 
    case x of
        ParserDrop -> Seq Drop (actionConversion xs)
        ParserAssignment variable expression ->  Seq (ActAssignment (variable : (assignmentConversion expression))) (actionConversion xs)
        FuncExpr funcexpression -> Seq (functionConversion funcexpression) (actionConversion xs)
        _ -> actionConversion xs

assignmentConversion :: ArithmeticExpression -> [String]
assignmentConversion (ArithVar name) = [name]
assignmentConversion (NumConstant number) = []
assignmentConversion (Negated arith) = assignmentConversion arith
assignmentConversion (Add arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Subtract arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Multiply arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Divide arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)

functionConversion :: FunctionExpression -> Program
functionConversion (Count (FuncVar name)) = SetHeaderValidity (name, Valid)
functionConversion (SetValid (FuncVar name)) = SetHeaderValidity (name, Valid)
functionConversion (SetInvalid (FuncVar name)) = SetHeaderValidity (name, Invalid)

------------------------------------- TABLE CONVERSION FUNCTIONS
tableConversion :: Statement -> ([Program], [Program], [Program]) -> ([Program], [Program], [Program])
tableConversion (ParserTable tableName (Keys k) (Acts act)) (actions, tables, prog) = 
    (actions, (tables ++ [Table tableName (keyConversion k) (actsConversion act actions)]), prog)

keyConversion :: [Variable] -> [String]
keyConversion [] = []
keyConversion (x:xs) =
    case x of 
        Var "none" -> keyConversion xs
        Var str -> str : keyConversion xs
        Semi str1 str2 -> str1 : keyConversion xs

actsConversion :: [String] -> [Program] -> [Program]
actsConversion [] actions = []
actsConversion (x:xs) actions = 
    (filter (\(ActCons id pr) -> if id == x then True else False) actions) ++ (actsConversion xs actions) 

------------------------------------- APPLY CONVERSION FUNCTIONS
applyConversion :: [Statement] -> [Program] -> [Program] -> Program
applyConversion [] actions tables = Skip
applyConversion (x:xs) actions tables =
    case x of
        ParserSkip -> Seq Skip (applyConversion xs actions tables)
        ParserIf (BoolExpr bexpr) (ParserSeq block) elseBlock ->
            case elseBlock of
                ParserSkip -> (Seq (If (condConversion bexpr) (applyConversion block actions tables) Skip) (applyConversion xs actions tables))
                ParserSeq eblock -> 
                    (Seq (If (condConversion bexpr) (applyConversion block actions tables) (applyConversion eblock actions tables)) (applyConversion xs actions tables))
        FuncExpr fexpr ->
            case fexpr of
                (ApplyFunc (FuncVar name)) -> (Seq (applyFuncConversion name tables) (applyConversion xs actions tables))
                (Emit (FuncVar name1) (FuncVar name2)) -> (Seq (Skip) (applyConversion xs actions tables))
                (ActionVar name) -> (Seq (actionCallConversion name actions) (applyConversion xs actions tables))
                _ -> Seq (functionConversion fexpr) (applyConversion xs actions tables)
        ParserAssignment variable aexpr -> (Seq (ActAssignment (variable : (assignmentConversion aexpr))) (applyConversion xs actions tables))
        _ -> applyConversion xs actions tables

condConversion :: BoolExpression -> [String]
condConversion (BoolVar name) = [name]
condConversion (BoolConstant bool) = []
condConversion (IsValid expr) = condConversion expr
condConversion (Not expr) = condConversion expr
condConversion (And expr1 expr2) = (condConversion expr1) ++ (condConversion expr2)
condConversion (Or expr1 expr2) = (condConversion expr1) ++ (condConversion expr2)
condConversion (Equal expr1 expr2) = (condConversion expr1) ++ (condConversion expr2)
condConversion (Inequal expr1 expr2) = (condConversion expr1) ++ (condConversion expr2)
condConversion (Greater expr1 expr2) = (condConversion expr1) ++ (condConversion expr2)
condConversion (Less expr1 expr2) = (condConversion expr1) ++ (condConversion expr2)

applyFuncConversion :: String -> [Program] -> Program
applyFuncConversion tableName tables 
    | result == [] = Skip
    | otherwise = head result
    where result = (filter (\(Table id keys progs) -> if id == tableName then True else False) tables)

emitConversion :: [Statement] -> Environment -> Environment
emitConversion [] final = final
emitConversion (x:xs) final =
    case x of
        FuncExpr (Emit (FuncVar name1) (FuncVar name2)) -> 
            emitConversion xs (prFunc_SetHeaderValidity final (drop 1 (dropWhile (/= '.') name2)) Valid)
        _ -> emitConversion xs final

actionCallConversion :: String -> [Program] -> Program
actionCallConversion actionName actions
    | result == [] = Skip
    | otherwise = head result
    where result = (filter (\(ActCons id pr) -> if id == actionName then True else False) actions)

------------------------------------- MISC FUNCTIONS
takeUntil :: String -> String -> String
takeUntil s [] = [] 
takeUntil [] ys = [] 
takeUntil s (y:ys) 
    | isPrefixOf s (y:ys) = []
    | otherwise = y : (takeUntil s (tail (y:ys)))

------------------------------------- VARIABLES

initEnv :: Environment
initEnv = Env [("drop",(Invalid, []))]

finalEnv :: Environment
finalEnv = Env [("drop",(Invalid, []))]

initProg :: [Program]
initProg = []

initActions :: [Program]
initActions = []

initTables :: [Program]
initTables = []