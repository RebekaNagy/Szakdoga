module Preparation where
import Data.String
import Data.List
import Parser
import Verification

------------------------------------- MAIN CONVERSION FUNCTIONS

mainConversion :: [Statement] -> (([Environment], Environment), ([Program], [Program], [Program]))-> (([IdEnvironment], [Environment]), Program)
mainConversion [] ((init, final), (actions, tables, prog)) =
    case filteredProgram of
        [] -> (((envToIdEnv init 0), finalizeFinal final), EmptyProg)
        _ -> (((envToIdEnv init 0), finalizeFinal final), (listToProgram filteredProgram))
    where filteredProgram = (filter (\x -> x /= Skip) prog)
mainConversion (x:xs) ((init, final), (actions, tables, prog)) =
    case initEnvValid init of
        False -> (([("", Stuck, EnvError)], [EnvError]), ProgError)
        True -> case final of
            EnvError -> (([("", Stuck, EnvError)], [EnvError]), ProgError)
            _ -> case prog of
                [ProgError] -> (((envToIdEnv init 0), finalizeFinal final), ProgError)
                _ -> case x of
                    ParserHeader name fields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
                    ParserStruct sname sfields -> mainConversion xs ((headerConversion x (init, final)), (actions, tables, prog))
                    Parser block -> mainConversion xs (((parserConversion x init), final), (actions, tables, prog))
                    Control cname cblock -> mainConversion xs (controlConversion cblock ((init, final), (actions, tables, prog)))
                    Error -> (((envToIdEnv init 0), finalizeFinal final), ProgError)
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
parserConversion (Parser parserBlock) init =
    case validParser of
        [] -> case containsStartState (Parser parserBlock) of 
                State "start" stateBlock -> stateConversion stateBlock (Parser parserBlock) init
                EmptyStatement -> [EnvError]
        _ -> [EnvError]
        where validParser = (filter (\element -> element == Error) parserBlock)

containsStartState :: Statement -> Statement
containsStartState (Parser block) =
    case contains of
        [] -> EmptyStatement
        _ -> head contains
    where contains = (filter (\element -> case element of
                                            State "start" stateBlock -> True
                                            _ -> False) block)


stateConversion :: Statement -> Statement -> [Environment] -> [Environment]
stateConversion (ParserSeq stateBlock) (Parser parserBlock) ((Env init):i) =
    case transitions of
        ["empty"] -> case transitionSelects of
                        ["empty"] -> case extracts of
                                        ["empty"] -> ((Env init):i)
                                        _ -> case validHeader of
                                            EnvError -> [EnvError]
                                            _ -> (validHeader:i)
                                            where validHeader = (extractHeader (Env init) extracts)
                                    where extracts = containsExtract stateBlock
                        _ -> case extracts of
                                ["empty"] -> selectingTransition transitionSelects (Parser parserBlock) ((Env init):i)
                                _ -> case validHeader of
                                    EnvError -> [EnvError]
                                    _ -> selectingTransition transitionSelects (Parser parserBlock) (validHeader:i)
                                    where validHeader = (extractHeader (Env init) extracts)
                            where extracts = containsExtract stateBlock
                    where transitionSelects = containsTrSelect stateBlock                   
        _ -> case extracts of
                ["empty"] -> nestingTransition transitions (Parser parserBlock) ((Env init):i)
                _ -> case validHeader of
                    EnvError -> [EnvError]
                    _ -> nestingTransition transitions (Parser parserBlock) (validHeader:i)
                    where validHeader = (extractHeader (Env init) extracts)
            where extracts = containsExtract stateBlock
    where transitions = containsTransition stateBlock

getTransition :: String -> [Statement] -> Statement
getTransition stateName parserBlock =
    case filterState of
        [] -> EmptyStatement
        _ -> case head filterState of
                State str stateBlock -> stateBlock
                _ -> EmptyStatement
    where filterState = (filter (\x -> case x of
                                        State str stateBlock -> str == stateName
                                        _ -> False) parserBlock)

selectingTransition :: [String] -> Statement -> [Environment] -> [Environment]
selectingTransition [] (Parser parserBlock) init = []
selectingTransition (trselect:xs) (Parser parserBlock) init =
    case trselect of
        "accept" -> init ++ (selectingTransition xs (Parser parserBlock) init)
        _ -> case stateBlock of
            EmptyStatement -> [EnvError]
            _ -> (stateConversion stateBlock (Parser parserBlock) init) ++ (selectingTransition xs (Parser parserBlock) init)
            where stateBlock = (getTransition trselect parserBlock)

nestingTransition :: [String] -> Statement -> [Environment] -> [Environment]
nestingTransition [] (Parser parserBlock) init = init
nestingTransition (transition:xs) (Parser parserBlock) init =
    case transition of
        "accept" -> init
        "reject" -> init
        _ -> case stateBlock of
                EmptyStatement -> [EnvError]
                _ -> nestingTransition xs (Parser parserBlock) (stateConversion stateBlock (Parser parserBlock) init)
                where stateBlock = (getTransition transition parserBlock)

extractHeader :: Environment -> [String] -> Environment
extractHeader (Env i) [] = (Env i)
extractHeader (Env i) (headerName:xs) = 
    case success of
        EnvError -> EnvError
        _ -> extractHeader success xs
    where success = (setToValid (Env i) headerName)

containsTrSelect :: [Statement] -> [String]
containsTrSelect stateBlock =
    case filterSelect of
        [] -> ["empty"]
        _ -> getTrSelectStrings filterSelect
        where filterSelect = (filter (\x -> case x of
                                            TransitionSelect str vars -> True
                                            _ -> False) stateBlock)

getTrSelectStrings :: [Statement] -> [String]
getTrSelectStrings [] = []
getTrSelectStrings ((TransitionSelect str vars):xs) = (map (\(Semi v1 v2) -> v2) vars) ++ getTrSelectStrings xs

containsTransition :: [Statement] -> [String]
containsTransition stateBlock =
    case filterTransition of
        [] -> ["empty"]
        _ -> getTrStrings filterTransition
        where filterTransition = (filter (\x -> case x of
                                            Transition str -> True
                                            _ -> False) stateBlock)

getTrStrings :: [Statement] -> [String]
getTrStrings [] = []
getTrStrings ((Transition str):xs) = str : getTrStrings xs

containsExtract :: [Statement] -> [String]
containsExtract stateBlock =
    case filterExtract of
        [] -> ["empty"]
        _ -> getExtractStrings filterExtract
        where filterExtract = (filter (\x -> case x of 
                                            FuncExpr (Extract (FuncVar pack) (FuncVar header)) -> True
                                            _ -> False) stateBlock)

getExtractStrings :: [Statement] -> [String]
getExtractStrings [] = []
getExtractStrings ((FuncExpr (Extract (FuncVar pack) (FuncVar header))):xs) = (drop 1 (dropWhile (/= '.') header)) : getExtractStrings xs


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
        ParserAssignment variable expression ->  Seq (Assignment (drop 1 (dropWhile (/= '.') variable)) (assignmentConversion expression)) (actionConversion xs)
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
functionConversion (Count (FuncVar name)) = SetHeaderValidity (drop 1 (dropWhile (/= '.') name)) Valid
functionConversion (SetValid (FuncVar name)) = SetHeaderValidity (drop 1 (dropWhile (/= '.') name)) Valid
functionConversion (SetInvalid (FuncVar name)) = SetHeaderValidity (drop 1 (dropWhile (/= '.') name)) Invalid

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
    case x of
        "NoAction" -> (ActCons "NoAction" Skip) : (actsConversion xs actions)
        _ -> case action of
            [] -> [] ++ (actsConversion xs actions)
            _ -> (head action) : (actsConversion xs actions)
        where action = (filter (\(ActCons id pr) -> id == x) actions)

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
        ParserAssignment variable aexpr -> (Seq (Assignment (drop 1 (dropWhile (/= '.') variable)) (assignmentConversion aexpr)) (applyConversion xs actions tables))
        _ -> applyConversion xs actions tables

condConversion :: BoolExpression -> [String]
condConversion (BoolVar name) 
    | newName == "" = []
    | otherwise = [newName]
    where newName = (drop 1 (dropWhile (/= '.') name))
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
    where result = (filter (\(Table id keys progs) -> id == tableName) tables)

emitConversion :: [Statement] -> Environment -> Environment
emitConversion [] final = final
emitConversion (x:xs) final =
    case x of
        FuncExpr (Emit (FuncVar name1) (FuncVar name2)) -> case validHeader of
            EnvError -> EnvError
            _ -> emitConversion xs validHeader
            where validHeader = (setToValid final (drop 1 (dropWhile (/= '.') name2))) 
        _ -> emitConversion xs final

actionCallConversion :: String -> [Program] -> Program
actionCallConversion actionName actions
    | result == [] = Skip
    | otherwise = head result
    where result = (filter (\(ActCons id pr) -> id == actionName) actions)

------------------------------------- SIDECONDITION CONVERSION FUNCTIONS

sideConditionConversion :: String -> SideCondition
sideConditionConversion (a:b:c:d:e:[])
    | ifc /= IfError && tablec /= TableError && assignc /= AssignmentError && sethc /= SetHeaderError && dropc /= DropError = 
        SideCon (ifc, tablec, assignc, sethc, dropc)
    | otherwise = SideCondError
    where { ifc = ifCondConversion a;
        tablec = tableCondConversion b;
        assignc = assignCondConversion c;
        sethc = setHeaderCondConversion d;
        dropc = dropCondConversion e
    }
sideConditionConversion _ = SideCondError

ifCondConversion :: Char -> IfCondition
ifCondConversion '0' = NoneIf
ifCondConversion '1' = CondsValid
ifCondConversion '2' = CondsInvalid
ifCondConversion _ = IfError

tableCondConversion :: Char -> TableCondition
tableCondConversion '0' = NoneTable 
tableCondConversion '1' = KeysValid
tableCondConversion '2' = KeysInvalid
tableCondConversion _ = TableError

assignCondConversion :: Char -> AssignmentCondition
assignCondConversion '0' = NoneAssignment 
assignCondConversion '1' = LeftValid
assignCondConversion '2' = LeftInvalid
assignCondConversion '3' = RightValid
assignCondConversion '4' = RightInvalid
assignCondConversion '5' = EveryAValid
assignCondConversion '6' = EveryAInvalid
assignCondConversion _ = AssignmentError

setHeaderCondConversion :: Char -> SetHeaderCondition
setHeaderCondConversion '0' = NoneSetHeader 
setHeaderCondConversion '1' = HeaderValid 
setHeaderCondConversion '2' = HeaderInvalid
setHeaderCondConversion '3' = FieldsValid
setHeaderCondConversion '4' = FieldsInvalid
setHeaderCondConversion '5' = EverySValid
setHeaderCondConversion '6' = EverySInvalid
setHeaderCondConversion _ = SetHeaderError

dropCondConversion :: Char -> DropCondition
dropCondConversion '0' = NoneDrop
dropCondConversion '1' = DropInvalid
dropCondConversion '2' = EveryHeaderValid
dropCondConversion '3' = EveryHeaderInvalid
dropCondConversion '4' = EveryFieldValid
dropCondConversion '5' = EveryFieldInvalid 
dropCondConversion _ = DropError


------------------------------------- MISC FUNCTIONS
setToValid :: Environment -> String -> Environment
setToValid EnvError header = EnvError
setToValid (Env env) header = 
    case isValidName of
        True -> Env (map (\x@(id, (v, f)) -> if id == header then (setEveryFieldValid x) else x) env)
        False -> EnvError
    where isValidName = or (map (\(id, (v, f)) -> id == header) env)

setEveryFieldValid :: Header -> Header
setEveryFieldValid (hid, (hv, fields)) = (hid, (Valid, (map (\(fid, fv) -> (fid, Valid)) fields)))

listToProgram :: [Program] -> Program
listToProgram [] = Skip
listToProgram [x] = x
listToProgram (x:xs) = Seq (x) (listToProgram xs)

initEnvValid :: [Environment] -> Bool
initEnvValid [] = True
initEnvValid (env:xs) =
    case env of
        EnvError -> False
        _ -> initEnvValid xs

envToIdEnv :: [Environment] -> Int -> [IdEnvironment]
envToIdEnv [] number = []
envToIdEnv ((env):xs) number = ((show number), NoMatch, env) : envToIdEnv xs (number + 1) 

finalizeFinal :: Environment -> [Environment]
finalizeFinal (Env env) = (Env env) : (dropEnv (Env env)) : []

dropEnv :: Environment -> Environment
dropEnv (Env env) = 
    Env (map (\(headerName, (headerValidity, fields)) -> 
        if headerName == "drop" then (headerName, (Valid, fields)) else (headerName, (Undefined, everyFieldUndifed fields))) env)

everyFieldUndifed :: [Field] -> [Field]
everyFieldUndifed [] = []
everyFieldUndifed ((fieldName, fieldValidity):xs) = (fieldName, Undefined) : everyFieldUndifed xs

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

dataToString :: Program -> String
dataToString (EmptyProg) = "A program nem tartalmaz verifikálásra alkalmas részt."
dataToString (ProgError) = "A program szintaktikailag helytelen, vagy a vizsgált résznyelven kívül esik."
dataToString (Skip) = "Skip "
dataToString (Seq pr1 pr2) = "Seq " ++ dataToString pr1 ++ " " ++ dataToString pr2
dataToString (If conds pr1 pr2) = "If " ++ (unwords conds) ++ dataToString pr1 ++ dataToString pr2  ++ " "
dataToString (Table str keys acts) = "Table " ++ str ++ " " ++ (unwords keys) ++ " " ++ (unwords (map (\x -> dataToString x) acts))
dataToString (ActCons str pr) = "Action " ++ str ++ " " ++ dataToString pr
dataToString (Assignment str strs) = "Assignment " ++ str ++ (unwords strs)
dataToString (Drop) = "Drop "
dataToString (SetHeaderValidity str v) = "SetHeader " ++ str ++ " " ++ (show v)


envListToString :: [IdEnvironment] -> String
envListToString [] = ""
envListToString ((id, envtype, env):xs) = id++"@"++(show envtype) ++"@"++(envToString env) ++ "#" ++ (envListToString xs)

envToString :: Environment -> String
envToString (Env []) = ""
envToString (Env (x:xs)) = headerToString x ++ "\n" ++ (envToString (Env xs))

headerToString :: Header -> String
headerToString (header, (validity, fields)) = "fejléc: (" ++ header ++ ", " ++ (show validity) ++ ")\n mezők:" ++ fieldsToString fields

fieldsToString :: [Field] -> String
fieldsToString [] = ""
fieldsToString ((name, validity):xs) = "(" ++ name ++ ", " ++ (show validity) ++ ") " ++ (fieldsToString xs) 

