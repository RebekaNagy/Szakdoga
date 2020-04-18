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
    | Assignment String [String]
    | Drop 
    | SetHeaderValidity String Validity deriving (Show, Eq)

data SideCondition = SideCon [String] deriving Show


type Rule = [Environment] -> Program -> SideCondition -> [Environment]
------------------------------------- MAIN VERIFICATION FUNCTION
verifyP4 :: [Environment] -> Program -> [Environment]
verifyP4 environmentList program = (fittingRule environmentList program initRules) environmentList program empSideCons
------------------------------------- PROGRAM FUNCTIONS

prFunc_Skip :: [Environment] -> [Environment]
prFunc_Skip envlist = envlist
--pl.: prFunc_Skip initEnv

prFunc_Drop :: [Environment] -> [Environment]
prFunc_Drop envlist = map (\(Env env) -> Env (map (\h@(header, (validity, fields)) -> if header == "drop" then (header, (Valid, fields)) else h) env)) envlist
--pl.: prFunc_Drop initEnv

prFunc_SetHeaderValidity :: [Environment] -> String -> Validity -> [Environment]
prFunc_SetHeaderValidity envlist header validity =
        map (\(Env env) -> Env (map (\h@(header', (validity', fields)) -> if header == header' then (header, (validity, fields)) else h) env)) envlist
--pl.: prFunc_SetHeaderValidity initEnv "ipv4" Valid

prFunc_SetFieldValidity :: [Environment] -> String -> [Environment]
prFunc_SetFieldValidity envlist fieldname = 
        map (\(Env env) -> Env (map (\header -> helper_SetFieldValidity header fieldname) env)) envlist
--pl.: prFunc_SetFieldValidity initEnv "dstAddr" Invalid

helper_SetFieldValidity :: Header -> String -> Header
helper_SetFieldValidity (headerName, (headerValidity, fields)) name = 
        (headerName, (headerValidity, (map (\field@(fieldName, fieldValidity) -> if fieldName == name then (fieldName, Valid) else field) fields)))

prFunc_Action :: [Environment] -> Program -> [Environment]
prFunc_Action envlist program = verifyP4 envlist program

prFunc_If :: [Environment] -> [String] -> Program -> Program -> [Environment]
prFunc_If envlist conditions ifprogram elseprogram = (verifyP4 envlist ifprogram) ++ (verifyP4 envlist elseprogram)

--If (SetHeaderValidity "ipv4" Valid) (Skip)

prFunc_Seq :: [Environment] -> Program -> Program -> [Environment]
prFunc_Seq envlist firstprogram secondprogram = verifyP4 (verifyP4 envlist firstprogram) secondprogram

prFunc_Table :: [Environment] -> [String] -> [Program] -> [Environment]
prFunc_Table envlist keys [] = []
prFunc_Table envlist keys (action:acts) = (verifyP4 envlist action) ++ (prFunc_Table envlist keys acts) 

------------------------------------- CALCULATING FUNCTIONS

fittingRule :: [Environment] -> Program -> [Rule] -> Rule
fittingRule envlist program [] = (\env ProgError sidecons -> prFunc_Skip envlist)
fittingRule envlist program (rule:xs) 
    | rule envlist program empSideCons == [EnvError] = fittingRule envlist program xs
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
        (\environmentList program sidecons -> case program of
                Drop -> prFunc_Drop environmentList
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                Skip -> prFunc_Skip environmentList
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                SetHeaderValidity str v -> (prFunc_SetHeaderValidity environmentList str v)
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                Assignment str strs -> (prFunc_SetFieldValidity environmentList str)
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                ActCons str pr -> prFunc_Action environmentList pr
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                If str pr1 pr2 -> prFunc_If environmentList str pr1 pr2
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                Seq pr1 pr2 -> prFunc_Seq environmentList pr1 pr2
                _ -> [EnvError]),
        (\environmentList program sidecons -> case program of
                Table str keys prs -> prFunc_Table environmentList keys prs
                _ -> [EnvError])
        ]

empSideCons :: SideCondition
empSideCons = SideCon []

exampleEnv :: [Environment]
exampleEnv = [Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Invalid),("srcAddr", Invalid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]]