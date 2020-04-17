module Verification where
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
    | ActAssignment String [String]
    | Drop 
    | SetHeaderValidity String Validity deriving (Show, Eq)

data SideCondition = SideCon [String] deriving Show


type Rule = ([Environment], Program) -> SideCondition -> ([Environment], Program)
------------------------------------- MAIN VERIFICATION FUNCTION
--verifyP4 :: ([Environment], Program) -> ([Environment], Program)
---verifyP4 EmptyProg envlist = [EnvError]
--verifyP4 ProgError envlist = [EnvError]
--verifyP4 Skip envlist = (fittingRule Skip envlist initRules) envlist Skip empSideCons
--verifyP4 (Seq program1 program2) envlist = envlist
--verifyP4 (If conds program1 program2) envlist = envlist
--verifyP4 (Table name keys actions) envlist = envlist
--verifyP4 (ActCons name action) envlist = (fittingRule (ActCons name action) envlist initRules) envlist (ActCons name action) empSideCons
--verifyP4 (ActAssignment left rights) envlist = (fittingRule (ActAssignment left rights) envlist initRules) envlist (ActAssignment left rights) empSideCons
--verifyP4 Drop envlist = (fittingRule Drop envlist initRules) envlist Drop empSideCons
--verifyP4 (SetHeaderValidity header validity) envlist = (fittingRule (SetHeaderValidity header validity) envlist initRules) envlist (SetHeaderValidity header validity) empSideCons
--(fittingRule Drop env initRules) env Drop empSideCons
------------------------------------- PROGRAM FUNCTIONS

prFunc_Skip :: ([Environment], Program) -> ([Environment], Program)
prFunc_Skip (envlist, program) = (envlist, program)
--pl.: prFunc_Skip initEnv

prFunc_Drop :: ([Environment], Program) -> ([Environment], Program)
prFunc_Drop (envlist, program) = (map (\(Env l) -> Env (map (\x@(id,(v, f)) -> if id == "drop" then (id,(Valid, f)) else x) l)) envlist, program)
--pl.: prFunc_Drop initEnv

prFunc_SetHeaderValidity :: ([Environment], Program) -> String -> Validity -> ([Environment], Program)
prFunc_SetHeaderValidity (envlist, program) id v = (map (\(Env l) -> Env (map (\x@(id', (v', f)) -> if id == id' then (id, (v, f)) else x) l)) envlist, program)
--pl.: prFunc_SetHeaderValidity initEnv "ipv4" Valid

prFunc_SetFieldValidity :: ([Environment], Program) -> String -> Validity-> ([Environment], Program)
prFunc_SetFieldValidity (envlist, program) id v = (map (\(Env l) -> Env (map (\x -> helper_SetFieldValidity x id v) l)) envlist, program)
--pl.: prFunc_SetFieldValidity initEnv "dstAddr" Invalid

helper_SetFieldValidity :: Header -> String -> Validity -> Header
helper_SetFieldValidity (hid, (hv, f)) id v = (hid, (hv, (map (\x@(id', v') -> if id' == id then (id, v) else x) f)))


prFunc_If :: ([Environment], Program) -> ([Environment], Program)
prFunc_If (envlist, program) = (envlist, program)

prFunc_Seq :: ([Environment], Program) -> ([Environment], Program)
prFunc_Seq (envlist, program) = (envlist, program)

prFunc_Table :: ([Environment], Program) -> ([Environment], Program)
prFunc_Table (envlist, program) = (envlist, program)

------------------------------------- CALCULATING FUNCTIONS

fittingRule :: [Environment] -> Program -> [Rule] -> Rule
fittingRule envlist program [] = (\(env, ProgError) sidecons -> prFunc_Skip (envlist, program))
fittingRule envlist program (rule:xs) 
    | rule (envlist, program) empSideCons == ([EnvError], ProgError) = fittingRule envlist program xs
    | otherwise = rule
--pl.: (fittingRule Drop env initRules) env Drop empSideCons

isValidSideCons :: SideCondition -> Environment -> Bool
isValidSideCons (SideCon []) _ = True 
isValidSideCons (SideCon (x:xs)) (Env l)
    | getValidity x (Env l) == Valid = (isValidSideCons (SideCon xs) (Env l))
    | otherwise = False

getValidity :: String -> Environment -> Validity
getValidity str (Env []) = Undefined
getValidity str (Env ((hid, (hv, fields)):xs))
    | hid == str = hv
    | helperResult == Undefined = getValidity str (Env xs)
    | otherwise = helperResult
    where helperResult = helper_getValidity str fields



helper_getValidity :: String -> [Field] -> Validity
helper_getValidity str [] = Undefined
helper_getValidity str (x:xs)
    | fst x == str = snd x
    | otherwise = helper_getValidity str xs


------------------------------------- RULES

initRules :: [Rule]
initRules = [
        (\(environmentList, pr) sidecons -> case pr of
                Drop -> prFunc_Drop (environmentList, pr)
                _ -> ([EnvError], ProgError)),
        (\(environmentList, pr) sidecons -> case pr of
                Skip -> prFunc_Skip (environmentList, pr)
                _ -> ([EnvError], ProgError)),
        (\(environmentList, pr) sidecons -> case pr of
                SetHeaderValidity str v -> (prFunc_SetHeaderValidity (environmentList, pr) str v)
                _ -> ([EnvError], ProgError)),
        (\(environmentList, pr) sidecons -> case pr of
                ActAssignment str strs -> (prFunc_SetFieldValidity (environmentList, pr) str Valid)
                _ -> ([EnvError], ProgError)),
        (\(environmentList, pr) sidecons -> case pr of
                If str pr1 pr2 -> prFunc_If (environmentList, pr)
                _ -> ([EnvError], ProgError)),
        (\(environmentList, pr) sidecons -> case pr of
                Seq pr1 pr2 -> prFunc_Seq (environmentList, pr)
                _ -> ([EnvError], ProgError)),
        (\(environmentList, pr) sidecons -> case pr of
                Table str keys prs -> prFunc_Table (environmentList, pr)
                _ -> ([EnvError], ProgError))
        ]

empSideCons :: SideCondition
empSideCons = SideCon []

exampleEnv :: Environment
exampleEnv = Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Valid),("srcAddr", Valid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]