module Preparation where
import Data.String
import Data.List
import Parser
import Verification

------------------------------------- MAIN CONVERSION FUNCTIONS

mainConversion :: [Statement] -> (([Environment], Environment), ([Program], [Program], [Program]))-> (([Environment], Environment), [Program])
mainConversion [] ((init, final), (actions, tables, prog)) =
    case filteredProgram of
        [] -> ((init, final), [EmptyProg])
        _ -> ((init, final), filteredProgram)
    where filteredProgram = (filter (\x -> x /= Skip) prog)
mainConversion (x:xs) ((init, final), (actions, tables, prog)) =
    case prog of
        [ProgError] -> ((init, final), [ProgError])
        _ -> case x of 
            ParserHeader name fields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
            ParserStruct sname sfields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
            Parser block -> mainConversion xs (((parserConversion x init), final), (actions, tables, prog))
            Control cname cblock -> mainConversion xs (controlConversion cblock ((init, final), (actions, tables, prog)))
            Error -> ((init, final), [ProgError])
            _ -> mainConversion xs ((init, final), (actions, tables, prog))

------------------------------------- HEADER CONVERSION FUNCTIONS

headerConversion :: Statement -> ([Environment], Environment) -> ([Environment], Environment)
headerConversion (ParserStruct name []) (init, Env final) = (init, Env final)
headerConversion (ParserHeader name []) (init, Env final) = (init, Env final)
headerConversion (ParserHeader name fields) (((Env init):i), Env final) = (((Env (init ++ [newInit])):i), Env (final ++ [newFinal]))
    where { newInit  = (name, (Invalid, fieldsConversion Invalid fields));
            newFinal = (name, (Undefined, fieldsConversion Undefined fields)) }
headerConversion (ParserStruct name ((StructField (structType, sname)):xs)) (((Env init):i), Env final) 
    | isItHeader structType (Env final) = rewriteHeader ((StructField (structType, sname)):xs) (((Env init):i), Env final)
    | otherwise = (((Env (init ++ [newInit])):i), Env (final ++ [newFinal]))
    where { newInit = (name, (Invalid, fieldsConversion Invalid ((StructField (structType, sname)):xs)));
            newFinal = (name, (Undefined, fieldsConversion Undefined ((StructField (structType, sname)):xs))) }

fieldsConversion :: Validity -> [Variable] -> [Field]
fieldsConversion validity [] = []
fieldsConversion validity ((ParserField (ptype, name)):xs) = (name, validity) : fieldsConversion validity xs
fieldsConversion validity ((StructField (ptype, name)):xs) = (name, validity) : fieldsConversion validity xs

isItHeader :: String -> Environment -> Bool
isItHeader id (Env l) = or (map (\x@(id', (v', f)) -> if id == id' then True else False) l)

rewriteHeader :: [Variable] -> ([Environment], Environment) -> ([Environment], Environment)
rewriteHeader [] (init, Env final) = (init, Env final)
rewriteHeader ((StructField (stype, sname)):xs) (((Env init):i), Env final) = rewriteHeader xs (((Env newInit):i), Env newFinal)
    where { newInit = (map (\x@(hname, (v, f)) -> if stype == hname then (sname, (v, (rewriteFields sname f))) else x) init);
            newFinal = (map (\x@(hname, (v, f)) -> if stype == hname then (sname, (v, (rewriteFields sname f))) else x) final) }
    
rewriteFields :: String -> [Field] -> [Field]
rewriteFields headerName fields = map (\(name, v) -> ((headerName ++ "." ++ name), v)) fields

------------------------------------- PARSER CONVERSION FUNCTIONS
parserConversion :: Statement -> [Environment] -> [Environment]
parserConversion (Parser []) init = init
parserConversion (Parser (x:xs)) init =
    case validParser of
        [] -> case containsStartState (Parser (x:xs)) of 
                ((State "start" block), True) -> stateConversion block init
                (EmptyStatement, False) -> [EnvError]
        _ -> [EnvError]
        where validParser = (filter (\element -> case element of
                                                    Error -> True
                                                    _ -> False) (x:xs))

containsStartState :: Statement -> (Statement, Bool)
containsStartState (Parser block) =
    case contains of
        [] -> (EmptyStatement, False)
        _ -> (head contains, True)
    where contains = (filter (\element -> case element of
                                            State "start" stateBlock -> True
                                            _ -> False) block)


--stateeConversion :: Statement -> Statement -> [Environment] -> [Environment]
--stateeConversion (ParserSeq []) (Parser block) init = init
--stateeConversion (ParserSeq (x:xs)) (Parser block) init =
--    case x of 
--        FuncExpr (Extract (FuncVar pack) (FuncVar header)) -> 
--            stateeConversion (ParserSeq xs) ((prFunc_SetHeaderValidity (prFunc_SetEveryFieldValidity (Env init) headername Valid) headername Valid):i)
--            where headername = (drop 1 (dropWhile (/= '.') header))
--        _ -> stateeConversion (ParserSeq xs) ((Env init):i)

stateConversion :: Statement -> [Environment] -> [Environment]
stateConversion (ParserSeq []) init = init 
stateConversion (ParserSeq (x:xs)) ((Env init):i) =
    case x of 
        Error -> [EnvError]
        FuncExpr (Extract (FuncVar pack) (FuncVar header)) -> 
            stateConversion (ParserSeq xs) ((prFunc_SetHeaderValidity (prFunc_SetEveryFieldValidity (Env init) headername Valid) headername Valid):i)
            where headername = (drop 1 (dropWhile (/= '.') header))
        _ -> stateConversion (ParserSeq xs) ((Env init):i)

------------------------------------- CONTROL CONVERSION FUNCTION
controlConversion :: [Statement] -> (([Environment], Environment), ([Program], [Program], [Program])) -> (([Environment], Environment), ([Program], [Program], [Program]))
controlConversion [] ((init, final),(actions, tables, prog)) = ((init, final),(actions, tables, prog))
controlConversion (x:xs) ((init, final),(actions, tables, prog)) =
    case x of
        Error -> ((init, final),(actions, tables, [ProgError]))
        ParserAction actionName (ParserSeq acts) -> case validActions of
            ProgError -> ((init, final),(actions, tables, [ProgError]))
            _ -> controlConversion xs ((init, final), ((actions ++ [ActCons actionName validActions]), tables, prog))
            where validActions = (actionConversion acts)
        ParserTable tableName (Keys k) (Acts act) -> controlConversion xs ((init, final), (tableConversion x (actions, tables, prog)))
        Apply (ParserSeq block) -> controlConversion xs ((init, (emitConversion block final)), (actions, tables, (prog ++ [applyConversion block actions tables])))
        _ -> controlConversion xs ((init, final), (actions, tables, prog))

------------------------------------- ACTION CONVERSION FUNCTIONS
actionConversion :: [Statement] -> Program
actionConversion [] = Skip
actionConversion (x:xs) = 
    case x of
        Error -> ProgError
        ParserDrop -> Seq Drop (actionConversion xs)
        ParserAssignment variable expression ->  Seq (ActAssignment (drop 1 (dropWhile (/= '.') variable)) (assignmentConversion expression)) (actionConversion xs)
        FuncExpr funcexpression -> case validExpr of
                ProgError -> ProgError
                _ -> Seq validExpr (actionConversion xs)
                where validExpr = functionConversion funcexpression
        _ -> actionConversion xs

assignmentConversion :: ArithmeticExpression -> [String]
assignmentConversion (ArithVar name) = [(drop 1 (dropWhile (/= '.') name))]
assignmentConversion (NumConstant number) = []
assignmentConversion (Negated arith) = assignmentConversion arith
assignmentConversion (Add arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Subtract arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Multiply arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)
assignmentConversion (Divide arith1 arith2) = (assignmentConversion arith1) ++ (assignmentConversion arith2)

functionConversion :: FunctionExpression -> Program
functionConversion (FuncVar name) = ProgError
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
        Var str -> (drop 1 (dropWhile (/= '.') str)) : keyConversion xs
        Semi str1 str2 -> (drop 1 (dropWhile (/= '.') str1)) : keyConversion xs

actsConversion :: [String] -> [Program] -> [Program]
actsConversion [] actions = []
actsConversion (x:xs) actions = 
    (filter (\(ActCons id pr) -> if id == x then True else False) actions) ++ (actsConversion xs actions) 

------------------------------------- APPLY CONVERSION FUNCTIONS
applyConversion :: [Statement] -> [Program] -> [Program] -> Program
applyConversion [] actions tables = Skip
applyConversion (x:xs) actions tables =
    case x of
        ParserSkip -> (applyConversion xs actions tables)
        ParserIf (BoolExpr bexpr) (ParserSeq block) elseBlock ->
            case elseBlock of
                ParserSkip -> (Seq (If (condConversion bexpr) (applyConversion block actions tables) Skip) (applyConversion xs actions tables))
                ParserSeq eblock -> 
                    (Seq (If (condConversion bexpr) (applyConversion block actions tables) (applyConversion eblock actions tables)) (applyConversion xs actions tables))
        FuncExpr fexpr ->
            case fexpr of
                (ApplyFunc (FuncVar name)) -> (Seq (applyFuncConversion name tables) (applyConversion xs actions tables))
                (Emit (FuncVar name1) (FuncVar name2)) -> (applyConversion xs actions tables)
                (ActionVar name) -> (Seq (actionCallConversion name actions) (applyConversion xs actions tables))
                _ -> Seq (functionConversion fexpr) (applyConversion xs actions tables)
        ParserAssignment variable aexpr -> (Seq (ActAssignment (drop 1 (dropWhile (/= '.') variable)) (assignmentConversion aexpr)) (applyConversion xs actions tables))
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
            emitConversion xs (prFunc_SetHeaderValidity (prFunc_SetEveryFieldValidity final headername Valid) headername Valid)
            where headername = (drop 1 (dropWhile (/= '.') name2))
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

initEnv :: [Environment]
initEnv = [Env [("drop",(Invalid, []))]]

finalEnv :: Environment
finalEnv = Env [("drop",(Invalid, []))]

initProg :: [Program]
initProg = []

initActions :: [Program]
initActions = []

initTables :: [Program]
initTables = []

programToString ::[Program] -> String
programToString [] = "\n"
programToString (x:xs) = dataToString x ++ programToString xs

dataToString :: Program -> String
dataToString (EmptyProg) = "A program nem tartalmaz verifikálásra alkalmas részt."
dataToString (ProgError) = "A program szintaktikailag helytelen, vagy a vizsgált résznyelven kívül esik."
dataToString (Skip) = "Skip "
dataToString (Seq pr1 pr2) = "Seq " ++ dataToString pr1 ++ " " ++ dataToString pr2
dataToString (If conds pr1 pr2) = "If " ++ (unwords conds) ++ dataToString pr1 ++ dataToString pr2  ++ " "
dataToString (Table str keys acts) = "Table " ++ str ++ " " ++ (unwords keys) ++ " " ++ (unwords (map (\x -> dataToString x) acts))
dataToString (ActCons str pr) = "Action " ++ str ++ " " ++ dataToString pr
dataToString (ActAssignment str strs) = "Assignment " ++ str ++ (unwords strs)
dataToString (Drop) = "Drop "
dataToString (SetHeaderValidity (str, v)) = "SetHeader " ++ str ++ " " ++ (validityToString v)
dataToString (SetFieldValidity str v) = "SetField " ++ str ++ " " ++ (validityToString v)

validityToString :: Validity -> String
validityToString Valid = "Valid"
validityToString Invalid = "Invalid"
validityToString Undefined = "Undefined"
