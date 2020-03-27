module Preparation where
import Data.String
import Data.List
import Parser
import Verification

------------------------------------- MAIN CONVERSION FUNCTION

mainConversion :: [Statement] -> ((Environment, Environment), ([Program], [Program], Program))-> ((Environment, Environment), Program)
mainConversion []  ((init, final), (actions, tables, prog)) = ((init, final), prog)
mainConversion (x:xs) ((init, final), (actions, tables, prog)) =
    case x of 
        ParserHeader name fields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
        ParserStruct sname sfields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
        Parser block -> mainConversion xs (((parserConversion x init), final), (actions, tables, prog))
        Control cname cblock -> mainConversion xs (controlConversion cblock ((init, final), (actions, tables, prog)))
        _ -> mainConversion xs ((init, final), (actions, tables, prog))

------------------------------------- HEADER CONVERSION FUNCTION

headerConversion :: Statement -> (Environment, Environment) -> (Environment, Environment)
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

------------------------------------- PARSER CONVERSION FUNCTION
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
controlConversion :: [Statement] -> ((Environment, Environment), ([Program], [Program], Program)) -> ((Environment, Environment), ([Program], [Program], Program))
controlConversion [] ((init, final),(actions, tables, prog)) = ((init, final),(actions, tables, prog))
controlConversion (x:xs) ((init, final),(actions, tables, prog)) =
    case x of 
        ParserAction actionName (ParserSeq acts) -> 
            controlConversion xs ((init, final), ((actions ++ [ActCons actionName (actionConversion acts)]), tables, prog))
        ParserTable tableName (Keys k) (Acts act) -> controlConversion xs ((init, final),(actions, tables, prog))
        Apply (ParserSeq block) -> controlConversion xs ((init, final),(actions, tables, prog))
        _ -> controlConversion xs ((init, final),(actions, tables, prog))

actionConversion :: [Statement] -> Program
actionConversion [] = Skip
actionConversion (x:xs) = 
    case x of
        ParserDrop -> Seq Drop (actionConversion xs)
        ParserAssignment variable expression ->  Seq (ActAssignment (variable : (assignmentConversion expression))) (actionConversion xs)
        FuncExpr funcexpression -> Seq (SetHeaderValidity (functionConversion funcexpression)) (actionConversion xs)
        _ -> actionConversion xs

assignmentConversion :: ArithmeticExpression -> [String]
assignmentConversion (ArithVar name) = [name]
assignmentConversion (NumConstant number) = []
assignmentConversion (Negated arith) = assignmentConversion arith
assignmentConversion (Add arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Subtract arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Multiply arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Divide arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)

functionConversion :: FunctionExpression -> (String, Validity)
functionConversion (Count (FuncVar name)) = (name, Valid)
functionConversion (SetValid (FuncVar name)) = (name, Valid)
functionConversion (SetInvalid (FuncVar name)) = (name, Invalid)


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

initProg :: Program
initProg = Skip

initActions :: [Program]
initActions = []

initTables :: [Program]
initTables = []