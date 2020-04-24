module Verification where
import Data.String
import Data.List
import System.IO
import System.Environment
import Debug.Trace
import System.IO.Unsafe
------------------------------------- TYPES
data Validity =  
    Valid
    | Invalid
    | Undefined deriving (Show)

instance Eq Validity where
    (==) Valid Invalid = False
    (==) Invalid Valid = False
    (==) _ _ = True

type Field = (String, Validity)

type Header = (String, (Validity, [Field]))

type IdEnvironment = (String, EnvType, Environment)

data EnvType = Match | NoMatch | Stuck deriving (Show, Eq)

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

data SideCondition = SideCon (IfCondition, TableCondition, AssignmentCondition, SetHeaderCondition, DropCondition) | SideCondError deriving (Show, Eq)

data IfCondition = 
    NoneIf
    | CondsValid
    | CondsInvalid 
    | IfError deriving (Show, Eq)

data TableCondition = 
    NoneTable 
    | KeysValid
    | KeysInvalid
    | TableError deriving (Show, Eq)

data AssignmentCondition = 
    NoneAssignment 
    | LeftValid
    | LeftInvalid
    | RightValid
    | RightInvalid
    | EveryAValid
    | EveryAInvalid 
    | AssignmentError deriving (Show, Eq)

data SetHeaderCondition = 
    NoneSetHeader 
    | HeaderValid 
    | HeaderInvalid
    | FieldsValid
    | FieldsInvalid
    | EverySValid
    | EverySInvalid  
    | SetHeaderError deriving (Show, Eq)

data DropCondition = 
    NoneDrop
    | DropInvalid
    | EveryHeaderValid
    | EveryHeaderInvalid
    | EveryFieldValid
    | EveryFieldInvalid 
    | DropError deriving (Show, Eq)

type Rule = [IdEnvironment] -> Program -> SideCondition -> Int -> [IdEnvironment]
------------------------------------- MAIN VERIFICATION FUNCTION

verifyP4 :: [IdEnvironment] -> Program -> SideCondition -> Int -> [IdEnvironment]
verifyP4 environmentList program sideconditions number = 
    ((fittingRule environmentList program sideconditions number initRules) environmentList program sideconditions (number))


------------------------------------- PROGRAM FUNCTIONS

prFunc_Skip :: [IdEnvironment] -> [IdEnvironment]
prFunc_Skip envlist = envlist

prFunc_Drop :: [IdEnvironment] -> SideCondition -> Int -> [IdEnvironment]
prFunc_Drop envlist (SideCon (_, _, _, _, dropCondition)) number =
    map (\(id, envtype, (Env env)) -> case envtype of
        Stuck -> (id, envtype, (Env env))
        _ -> if (check_Drop (Env env) dropCondition) then (id++"$drop"++(show number), NoMatch, Env (map (\h@(header, (validity, fields)) -> 
            if header == "drop" then (header, (Valid, fields)) else h) env)) else (id, Stuck, (Env env))) envlist

prFunc_SetHeaderValidity :: [IdEnvironment] -> String -> Validity -> SideCondition -> [IdEnvironment]
prFunc_SetHeaderValidity envlist header validity (SideCon (_, _, _, setHeaderCondition, _)) =
    map (\(id, envtype, (Env env)) -> case envtype of
        Stuck -> (id, envtype, (Env env))
        _ -> if (check_SetHeader (Env env) setHeaderCondition header) then (id++"$setHeader:"++header, NoMatch, Env (map (\h@(header', (validity', fields)) -> 
                if header == header' then (header, (validity, helper_SetHeaderValidity fields)) else h) env)) else (id, Stuck, (Env env))) envlist

helper_SetHeaderValidity :: [Field] -> [Field]
helper_SetHeaderValidity [] = []
helper_SetHeaderValidity ((fieldName, fieldValidity):xs) = (fieldName, Invalid) : helper_SetHeaderValidity xs

prFunc_Assignment :: [IdEnvironment] -> String -> [String] -> SideCondition -> [IdEnvironment]
prFunc_Assignment envlist left rights (SideCon (_, _, assignmentCondition, _, _)) = 
    map (\(id, envtype, (Env env)) -> case envtype of
        Stuck -> (id, envtype, (Env env))
        _ -> if (check_Assignment (Env env) assignmentCondition left rights) then (id++"$assignment:"++left, NoMatch, Env (map (\h@(header, (validity, fields)) -> 
            if header == left then (header, (Valid, fields)) else helper_Assignment h left) env)) else (id, Stuck, (Env env))) envlist

helper_Assignment :: Header -> String -> Header
helper_Assignment (headerName, (headerValidity, fields)) name = 
    (headerName, (headerValidity, (map (\field@(fieldName, fieldValidity) -> if fieldName == name then (fieldName, Valid) else field) fields)))

prFunc_Action :: [IdEnvironment] -> String -> Program -> SideCondition -> Int -> [IdEnvironment]
prFunc_Action envlist name program sideconditions number = 
    (verifyP4 (map (\(id, envtype, env) -> (id++"$action:"++name, envtype, env)) envlist) program sideconditions (number+1))

prFunc_If :: [IdEnvironment] -> [String] -> Program -> Program -> SideCondition -> Int -> [IdEnvironment]
prFunc_If envlist conditions ifprogram elseprogram sideconditions@(SideCon (ifCondition, _, _, _, _)) number= 
    (verifyP4 (map (\(id, envtype, env) -> case envtype of
                        Stuck -> (id, envtype, env)
                        _ -> if (check_If env ifCondition conditions) 
                            then (id++"$if"++(show number), NoMatch, env) else (id, Stuck, env)) envlist) ifprogram sideconditions (number+1)) 
                        ++ 
    (verifyP4 (map (\(id, envtype, env) -> case envtype of
                        Stuck -> (id, envtype, env)
                        _ -> if (check_If env ifCondition conditions) 
                            then (id++"$else"++(show number), NoMatch, env) else (id, Stuck, env)) envlist) elseprogram sideconditions (number+1))

prFunc_Seq :: [IdEnvironment] -> Program -> Program -> SideCondition -> Int -> [IdEnvironment]
prFunc_Seq envlist firstprogram secondprogram sideconditions number = (verifyP4 ((verifyP4 envlist firstprogram sideconditions number)) secondprogram sideconditions number)

prFunc_Table :: [IdEnvironment] -> String -> [String] -> [Program] -> SideCondition -> Int -> [IdEnvironment]
prFunc_Table envlist name keys [] sideconditions@(SideCon (_, tableCondition, _, _, _)) number = []
prFunc_Table envlist name keys (action:acts) sideconditions@(SideCon (_, tableCondition, _, _, _)) number = 
    (verifyP4 newEnvlist action sideconditions number) ++ (prFunc_Table envlist name keys acts sideconditions number) 
    where newEnvlist = (map (\(id, envtype, env) -> case envtype of
                        Stuck -> (id, envtype, env)
                        _ -> if (check_Table env tableCondition keys) then (id++"$table:"++name, NoMatch, env) else (id, Stuck, env)) envlist)

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
        EverySValid -> getHeaderValidity header (Env env) == Valid && and (concat (map (\(h, (v, fields)) -> if h == header then (map (\(fieldName, fieldValidity) -> if fieldValidity == Valid then True else False) fields) else [True]) env))
        EverySInvalid -> getHeaderValidity header (Env env) == Invalid && and (concat (map (\(h, (v, fields)) -> if h == header then (map (\(fieldName, fieldValidity) -> if fieldValidity == Invalid then True else False) fields) else [True]) env))

check_Assignment :: Environment -> AssignmentCondition -> String -> [String] -> Bool
check_Assignment (Env env) assignmentCondition left rights =
    case assignmentCondition of
        NoneAssignment -> True
        LeftValid -> getValidity left (Env env) == Valid
        LeftInvalid -> getValidity left (Env env) == Invalid
        RightValid -> getListValidity (Env env) rights Valid
        RightInvalid -> getListValidity (Env env) rights Invalid
        EveryAValid -> (getValidity left (Env env) == Valid) && (getListValidity (Env env) rights Valid)
        EveryAInvalid -> (getValidity left (Env env) == Invalid) && (getListValidity (Env env) rights Invalid)

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

fittingRule :: [IdEnvironment] -> Program -> SideCondition -> Int -> [Rule] -> Rule
fittingRule envlist program sideconditions number [] = (\env ProgError sideconditions number -> prFunc_Skip envlist)
fittingRule envlist program sideconditions number (rule:xs) 
    | rule envlist program sideconditions number == [("", Stuck, EnvError)] = fittingRule envlist program sideconditions number xs
    | otherwise = rule

getHeaderValidity :: String -> Environment -> Validity
getHeaderValidity name (Env []) = Undefined
getHeaderValidity name (Env ((headerName, (headerValidity, fields)):xs))
    | headerName == name = headerValidity
    | otherwise = getHeaderValidity name (Env xs)

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

------------------------------------- COMPARATIVE FUNCTIONS

compareCalculatedWithFinal :: [IdEnvironment] -> [Environment] -> [IdEnvironment]
compareCalculatedWithFinal [] finalenvs = []
compareCalculatedWithFinal (idEnv:xs) finalenvs = helper_compareCalWithFinal idEnv finalenvs : compareCalculatedWithFinal xs finalenvs

helper_compareCalWithFinal :: IdEnvironment -> [Environment] -> IdEnvironment
helper_compareCalWithFinal (id, envtype, env) [] = (id, envtype, env)
helper_compareCalWithFinal (id, envtype, env) (finalenv:xs) =
    case envtype of
        NoMatch -> if env == finalenv then helper_compareCalWithFinal (id, Match, env) xs else helper_compareCalWithFinal (id, NoMatch, env) xs
        _ -> helper_compareCalWithFinal (id, envtype, env) xs

------------------------------------- RULES

initRules :: [Rule]
initRules = [
        (\environmentList program sideconditions number -> case program of
                Drop -> prFunc_Drop environmentList sideconditions number
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                Skip -> prFunc_Skip environmentList
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                SetHeaderValidity str v -> (prFunc_SetHeaderValidity environmentList str v sideconditions)
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                Assignment str strs -> (prFunc_Assignment environmentList str strs sideconditions)
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                ActCons str pr -> prFunc_Action environmentList str pr sideconditions number
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                If str pr1 pr2 -> prFunc_If environmentList str pr1 pr2 sideconditions number
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                Seq pr1 pr2 -> prFunc_Seq environmentList pr1 pr2 sideconditions number
                _ -> [("", Stuck, EnvError)]),
        (\environmentList program sideconditions number -> case program of
                Table str keys prs -> prFunc_Table environmentList str keys prs sideconditions number
                _ -> [("", Stuck, EnvError)])
        ]

empSideCons :: SideCondition
empSideCons = SideCon (NoneIf, NoneTable, NoneAssignment, NoneSetHeader, NoneDrop)

exampleEnv :: [Environment]
exampleEnv = [Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("ipv4.dstAddr", Invalid),("ipv4.srcAddr", Invalid)])),
        ("ethernet", (Valid, [("ethernet.field1", Valid),("ethernet.field2", Valid)]))],
        Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Valid, [("ipv4.dstAddr", Valid),("ipv4.srcAddr", Valid)])),
        ("ethernet", (Invalid, [("ethernet.field1", Invalid),("ethernet.field2", Invalid)]))]]

exampleEnv2 :: [IdEnvironment]
exampleEnv2 = [("0", Match, Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("ipv4.dstAddr", Undefined),("ipv4.srcAddr", Undefined)])),
        ("ethernet", (Valid, [("ethernet.field1", Valid),("ethernet.field2", Valid)]))]),
        ("1", NoMatch, Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Valid, [("ipv4.dstAddr", Undefined),("ipv4.srcAddr", Undefined)])),
        ("ethernet", (Invalid, [("ethernet.field1", Invalid),("ethernet.field2", Invalid)]))])]

exampleHeader :: [Header]
exampleHeader = [
        ("drop", (Invalid, [])), 
        ("ipv4", (Valid, [("ipv4.dstAddr", Valid),("ipv4.srcAddr", Valid)])),
        ("ethernet", (Valid, [("ethernet.field1", Valid),("ethernet.field2", Valid)]))]

exampleProg :: Program
exampleProg = Skip