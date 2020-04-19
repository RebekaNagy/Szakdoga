module Verification where
import Data.String
import Data.List
------------------------------------- TYPES
data Validity =  
    Valid
    | Invalid
    | Undefined deriving (Show, Eq) 

type Field = (String, Validity)

type Header = (String, (Validity, [Field]))

data Environment = Env [Header] | EnvError deriving (Show, Eq)

data Program =
    EmptyProg
    | ProgError
    | Skip
    | Seq Program Program
    | If [String] Program Program
    | Table String [String] [Program]
    | ActCons String Program
    | Assignment String [String]
    | Drop 
    | SetHeaderValidity String Validity deriving (Show, Eq)

data SideCondition = SideCon (IfCondition, TableCondition, AssignmentCondition, SetHeaderCondition, DropCondition) deriving (Show, Eq)

data IfCondition = 
        NoneIf
        | CondsValid
        | CondsInvalid deriving (Show, Eq)

data TableCondition = 
        NoneTable 
        | KeysValid
        | KeysInvalid deriving (Show, Eq)

data AssignmentCondition = 
        NoneAssignment 
        | LeftValid
        | LeftInvalid
        | RightValid
        | RightInvalid
        | EveryValid
        | EveryInvalid deriving (Show, Eq)

data SetHeaderCondition = 
        NoneSetHeader 
        | HeaderValid 
        | HeaderInvalid
        | FieldsValid
        | FieldsInvalid deriving (Show, Eq)

data DropCondition = 
        NoneDrop 
        | DropValid
        | DropInvalid
        | EveryHeaderValid
        | EveryHeaderInvalid
        | EveryFieldValid
        | EveryFieldInvalid deriving (Show, Eq)

type Rule = [Environment] -> Program -> SideCondition -> [Environment]
------------------------------------- MAIN VERIFICATION FUNCTION
verifyP4 :: [Environment] -> Program -> SideCondition -> [Environment]
verifyP4 environmentList program sideconditions = (fittingRule environmentList program sideconditions initRules) environmentList program sideconditions
------------------------------------- PROGRAM FUNCTIONS

prFunc_Skip :: [Environment] -> [Environment]
prFunc_Skip envlist = envlist

prFunc_Drop :: [Environment] -> SideCondition -> [Environment]
prFunc_Drop envlist (SideCon (_, _, _, _, dropCondition)) =
    map (\(Env env) -> 
        if (check_Drop (Env env) dropCondition) then Env (map (\h@(header, (validity, fields)) -> 
            if header == "drop" then (header, (Valid, fields)) else h) env) else (Env env)) envlist

prFunc_SetHeaderValidity :: [Environment] -> String -> Validity -> SideCondition -> [Environment]
prFunc_SetHeaderValidity envlist header validity (SideCon (_, _, _, setHeaderCondition, _)) =
    map (\(Env env) -> 
            if (check_SetHeader (Env env) setHeaderCondition header) then Env (map (\h@(header', (validity', fields)) -> 
                if header == header' then (header, (validity, fields)) else h) env) else (Env env)) envlist

prFunc_Assignment :: [Environment] -> String -> [String] -> SideCondition -> [Environment]
prFunc_Assignment envlist left rights (SideCon (_, _, assignmentCondition, _, _)) = 
    map (\(Env env) -> 
        if (check_Assignment (Env env) assignmentCondition left rights) then Env (map (\h@(header, (validity, fields)) -> 
            if header == left then (header, (Valid, fields)) else helper_Assignment h left) env) else (Env env)) envlist

helper_Assignment :: Header -> String -> Header
helper_Assignment (headerName, (headerValidity, fields)) name = 
    (headerName, (headerValidity, (map (\field@(fieldName, fieldValidity) -> if fieldName == name then (fieldName, Valid) else field) fields)))

prFunc_Action :: [Environment] -> Program -> SideCondition -> [Environment]
prFunc_Action envlist program sideconditions = verifyP4 envlist program sideconditions

prFunc_If :: [Environment] -> [String] -> Program -> Program -> SideCondition -> [Environment]
prFunc_If envlist conditions ifprogram elseprogram sideconditions@(SideCon (ifCondition, _, _, _, _)) = 
    (verifyP4 envlist ifprogram sideconditions) ++ (verifyP4 envlist elseprogram sideconditions)

prFunc_Seq :: [Environment] -> Program -> Program -> SideCondition -> [Environment]
prFunc_Seq envlist firstprogram secondprogram sideconditions = verifyP4 (verifyP4 envlist firstprogram sideconditions) secondprogram sideconditions

prFunc_Table :: [Environment] -> [String] -> [Program] -> SideCondition -> [Environment]
prFunc_Table envlist keys [] sideconditions@(SideCon (_, tableCondition, _, _, _)) = []
prFunc_Table envlist keys (action:acts) sideconditions = (verifyP4 envlist action sideconditions) ++ (prFunc_Table envlist keys acts sideconditions) 

------------------------------------- CHECKING FUNCTIONS

check_Drop :: Environment -> DropCondition -> Bool
check_Drop (Env env) dropCondition =
    case dropCondition of
        NoneDrop -> True
        DropInvalid -> getHeaderValidity "drop" (Env env) == Invalid
        EveryHeaderValid -> and (map (\(headerName, (headerValidity, fields)) -> if (headerName == "drop" || headerValidity == Valid) then True else False) env)
        EveryHeaderInvalid -> and (map (\(headerName, (headerValidity, fields)) -> if (headerName == "drop" || headerValidity == Invalid) then True else False) env)
        EveryFieldValid -> and (concat (map (\(h, (v, fields)) -> map (\(fieldName, fieldValidity) -> if fieldValidity == Valid then True else False) fields) env))
        EveryFieldInvalid -> and (concat (map (\(h, (v, fields)) -> map (\(fieldName, fieldValidity) -> if fieldValidity == Invalid then True else False) fields) env))

check_SetHeader :: Environment -> SetHeaderCondition -> String -> Bool
check_SetHeader (Env env) setHeaderCondition header = 
    case setHeaderCondition of
        NoneSetHeader -> True 
        HeaderValid -> getHeaderValidity header (Env env) == Valid
        HeaderInvalid -> getHeaderValidity header (Env env) == Invalid
        FieldsValid -> and (concat (map (\(h, (v, fields)) -> if h == header then (map (\(fieldName, fieldValidity) -> if fieldValidity == Valid then True else False) fields) else [True]) env))
        FieldsInvalid -> and (concat (map (\(h, (v, fields)) -> if h == header then (map (\(fieldName, fieldValidity) -> if fieldValidity == Invalid then True else False) fields) else [True]) env))

check_Assignment :: Environment -> AssignmentCondition -> String -> [String] -> Bool
check_Assignment (Env env) assignmentCondition left rights =
    case assignmentCondition of
        NoneAssignment -> True
        LeftValid -> getValidity left (Env env) == Valid
        LeftInvalid -> getValidity left (Env env) == Invalid
        RightValid -> getListValidity (Env env) rights Valid
        RightInvalid -> getListValidity (Env env) rights Invalid
        EveryValid -> (getValidity left (Env env) == Valid) && (getListValidity (Env env) rights Valid)
        EveryInvalid -> (getValidity left (Env env) == Invalid) && (getListValidity (Env env) rights Invalid)

check_Table :: Environment -> TableCondition -> [String] -> Bool
check_Table (Env env) tableCondition keys =
    case tableCondition of
        NoneTable -> True
        KeysValid -> getListValidity (Env env) keys Valid
        KeysInvalid -> getListValidity (Env env) keys Invalid

check_If :: Environment -> IfCondition -> [String] -> Bool
check_If (Env env) ifCondition conditions =
    case ifCondition of
        NoneIf -> True
        CondsValid -> getListValidity (Env env) conditions Valid
        CondsInvalid -> getListValidity (Env env) conditions Invalid

------------------------------------- CALCULATING FUNCTIONS
 
getListValidity :: Environment -> [String] -> Validity -> Bool
getListValidity environment list validity = and (map (\elem -> if (getValidity elem environment) == validity then True else False) list)

fittingRule :: [Environment] -> Program -> SideCondition -> [Rule] -> Rule
fittingRule envlist program sideconditions [] = (\env ProgError sideconditions -> prFunc_Skip envlist)
fittingRule envlist program sideconditions (rule:xs) 
    | rule envlist program sideconditions == [EnvError] = fittingRule envlist program sideconditions xs
    | otherwise = rule

getHeaderValidity :: String -> Environment -> Validity
getHeaderValidity name (Env []) = Undefined
getHeaderValidity name (Env ((headerName, (headerValidity, fields)):xs))
    | headerName == name = headerValidity
    | otherwise = getHeaderValidity name (Env xs)

getFieldValidity :: String -> Environment -> Validity
getFieldValidity name (Env []) = Undefined
getFieldValidity name (Env ((headerName, (headerValidity, fields)):xs))
    | (takeUntil "." name) == headerName = helper_getValidity name fields
    | otherwise = getFieldValidity name (Env xs)

getValidity :: String -> Environment -> Validity
getValidity name (Env []) = Undefined
getValidity name (Env ((headerName, (headerValidity, fields)):xs))
    | headerName == name = headerValidity
    | helperResult == Undefined = getValidity name (Env xs)
    | otherwise = helperResult
    where helperResult = helper_getValidity name fields

helper_getValidity :: String -> [Field] -> Validity
helper_getValidity name [] = Undefined
helper_getValidity name (field:xs)
    | fst field == name = snd field
    | otherwise = helper_getValidity name xs

takeUntil :: String -> String -> String
takeUntil s [] = [] 
takeUntil [] ys = [] 
takeUntil s (y:ys) 
    | isPrefixOf s (y:ys) = []
    | otherwise = y : (takeUntil s (tail (y:ys)))

------------------------------------- RULES

initRules :: [Rule]
initRules = [
        (\environmentList program sideconditions -> case program of
                Drop -> prFunc_Drop environmentList sideconditions
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                Skip -> prFunc_Skip environmentList
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                SetHeaderValidity str v -> (prFunc_SetHeaderValidity environmentList str v sideconditions)
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                Assignment str strs -> (prFunc_Assignment environmentList str strs sideconditions)
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                ActCons str pr -> prFunc_Action environmentList pr sideconditions
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                If str pr1 pr2 -> prFunc_If environmentList str pr1 pr2 sideconditions
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                Seq pr1 pr2 -> prFunc_Seq environmentList pr1 pr2 sideconditions
                _ -> [EnvError]),
        (\environmentList program sideconditions -> case program of
                Table str keys prs -> prFunc_Table environmentList keys prs sideconditions
                _ -> [EnvError])
        ]

empSideCons :: SideCondition
empSideCons = SideCon (NoneIf, NoneTable, RightValid, FieldsInvalid, EveryHeaderValid)

exampleEnv :: [Environment]
exampleEnv = [Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("ipv4.dstAddr", Invalid),("ipv4.srcAddr", Invalid)])),
        ("ethernet", (Valid, [("ethernet.field1", Valid),("ethernet.field2", Valid)]))],
        Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Valid, [("ipv4.dstAddr", Valid),("ipv4.srcAddr", Valid)])),
        ("ethernet", (Invalid, [("ethernet.field1", Invalid),("ethernet.field2", Invalid)]))]]

exampleHeader :: [Header]
exampleHeader = [
        ("drop", (Invalid, [])), 
        ("ipv4", (Valid, [("ipv4.dstAddr", Valid),("ipv4.srcAddr", Valid)])),
        ("ethernet", (Valid, [("ethernet.field1", Valid),("ethernet.field2", Valid)]))]