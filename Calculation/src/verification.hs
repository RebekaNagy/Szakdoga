module Verification where
import Data.String
import Data.List
import System.IO
import System.Environment
import Debug.Trace
import System.IO.Unsafe
------------------------------------- TYPES
data Validity =
    None
    | Valid
    | Invalid
    | Undefined deriving (Show)

instance Eq Validity where
    (==) Valid Valid = True
    (==) Invalid Invalid = True
    (==) None None = True
    (==) Undefined _ = True    
    (==) _ Undefined = True    
    (==) _ _ = False

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

data SideCondition = SideCon ([Validity], [Validity], [Validity], [Validity], [Validity]) | SideCondError deriving (Show, Eq)

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

check_Drop :: Environment -> [Validity] -> Bool
check_Drop env (None:None:None:[]) = True
check_Drop env (dropvalidity:fieldsValidity:headersValidity:[]) =
    (isHeaderValidity "drop" env dropvalidity) && (isdropAllFieldValidity env fieldsValidity) && (isdropAllHeaderValidity env headersValidity)
    
check_SetHeader :: Environment -> [Validity] -> String -> Bool
check_SetHeader env (None:None:[]) headerName = True
check_SetHeader env (fieldsValidity:headerValidity:[]) headerName = 
    (isAllFieldsValidity headerName env fieldsValidity) && (isHeaderValidity headerName env headerValidity)

check_Assignment :: Environment -> [Validity] -> String -> [String] -> Bool
check_Assignment env (None:None:None:None:[]) left rights = True
check_Assignment env (leftfield:leftheader:rightfields:rightheaders:[]) left rights =
    (isFieldValidity left env leftfield) && (isHeaderValidity left env leftheader) && 
    (isFieldListValidity rights env rightfields) && (isHeaderListValidity rights env rightheaders)

check_Table :: Environment -> [Validity] -> [String] -> Bool
check_Table env (None:None:[]) keys = True
check_Table env (fieldValidity:headerValidity:[]) keys = 
    (isFieldListValidity keys env fieldValidity) && (isHeaderListValidity keys env headerValidity)

check_If :: Environment -> [Validity] -> [String] -> Bool
check_If env (None:None:[]) conditions = True
check_If env (fieldValidity:headerValidity:[]) conditions = 
    (isFieldListValidity conditions env fieldValidity) && (isHeaderListValidity conditions env headerValidity)
------------------------------------- CALCULATING FUNCTIONS
 
isdropAllFieldValidity :: Environment -> Validity -> Bool
isdropAllFieldValidity env None = True
isdropAllFieldValidity (Env env) validity = and (map (\(hName, (hValidity, fields)) -> helper_isAllFieldsValidity fields validity) env)

isdropAllHeaderValidity :: Environment -> Validity -> Bool
isdropAllHeaderValidity env None = True
isdropAllHeaderValidity (Env env) validity = and (map (\(hName, (hValidity, fields)) -> hValidity == validity) env)

isFieldListValidity :: [String] -> Environment -> Validity -> Bool
isFieldListValidity list env None = True 
isFieldListValidity list env validity = and (map (\elem -> (isFieldValidity elem env validity)) list)

isHeaderListValidity :: [String] -> Environment -> Validity -> Bool
isHeaderListValidity list env None = True 
isHeaderListValidity list env validity = and (map (\elem -> (isHeaderValidity (takeUntil "." elem) env validity)) list)

isHeaderValidity :: String -> Environment -> Validity -> Bool
isHeaderValidity name env None = True
isHeaderValidity name (Env []) validity = False
isHeaderValidity name (Env ((headerName, (headerValidity, fields)):xs)) validity
    | headerName == name = headerValidity == validity
    | otherwise = isHeaderValidity name (Env xs) validity

isFieldValidity :: String -> Environment -> Validity -> Bool
isFieldValidity name env None = True
isFieldValidity name (Env []) validity = False
isFieldValidity name (Env ((headerName, (headerValidity, fields)):xs)) validity
    | helperResult == None = isFieldValidity name (Env xs) validity
    | otherwise = helperResult == validity
    where helperResult = helper_isFieldValidity name fields

helper_isFieldValidity :: String -> [Field] -> Validity
helper_isFieldValidity name [] = None
helper_isFieldValidity name ((fieldName, fieldValidity):xs)
    | fieldName == name = fieldValidity
    | otherwise = helper_isFieldValidity name xs

isAllFieldsValidity :: String -> Environment -> Validity -> Bool
isAllFieldsValidity name env None = True
isAllFieldsValidity name (Env ((headerName, (headerValidity, fields)):xs)) validity
    | name == headerName = helper_isAllFieldsValidity fields validity
    | otherwise = isAllFieldsValidity name (Env xs) validity

helper_isAllFieldsValidity :: [Field] -> Validity -> Bool
helper_isAllFieldsValidity [] validity = True
helper_isAllFieldsValidity ((fieldName, fieldValidity):xs) validity
    | fieldValidity == validity = helper_isAllFieldsValidity xs validity
    | otherwise = False

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

fittingRule :: [IdEnvironment] -> Program -> SideCondition -> Int -> [Rule] -> Rule
fittingRule envlist program sideconditions number [] = (\env ProgError sideconditions number -> prFunc_Skip envlist)
fittingRule envlist program sideconditions number (rule:xs) 
    | rule envlist program sideconditions number == [("", Stuck, EnvError)] = fittingRule envlist program sideconditions number xs
    | otherwise = rule


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
empSideCons = SideCon ([None, None], [None, None], [None, None, None, None], [None, None], [None, None, None])

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