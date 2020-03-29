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
    PrError
    | Skip
    | Seq Program Program
    | If [String] Program Program
    | Table String [String] [Program]
    | ActCons String Program
    | ActAssignment [String]
    | Drop 
    | SetHeaderValidity (String, Validity)
    | SetFieldValidity String Validity deriving (Show, Eq)

data SideCondition = SideCon [String] deriving Show


type Rule = Environment -> Program -> SideCondition -> Environment

------------------------------------- PROGRAM FUNCTIONS

prFunc_Skip :: Environment -> Environment
prFunc_Skip (Env x) = (Env x)
--pl.: prFunc_Skip initEnv

prFunc_Drop :: Environment -> Environment
prFunc_Drop (Env l) = Env (map (\x@(id,(v, f)) -> if id == "drop" then (id,(Valid, f)) else x) l)
--pl.: prFunc_Drop initEnv

prFunc_SetHeaderValidity :: Environment -> String -> Validity -> Environment
prFunc_SetHeaderValidity (Env l) id v = Env (map (\x@(id', (v', f)) -> if id == id' then (id, (v, f)) else x) l)
--pl.: prFunc_SetHeaderValidity initEnv "ipv4" Valid

prFunc_SetFieldValidity :: Environment -> String -> Validity-> Environment
prFunc_SetFieldValidity (Env l) id v = Env (map (\x -> helper_SetFieldValidity x id v) l)
--pl.: prFunc_SetFieldValidity initEnv "dstAddr" Invalid

prFunc_If :: Environment -> Environment
prFunc_If (Env l) = (Env l)

prFunc_Seq :: Environment -> Environment
prFunc_Seq (Env l) = (Env l)

prFunc_Table :: Environment -> Environment
prFunc_Table (Env l) = (Env l)

------------------------------------- CALCULATING FUNCTIONS

fittingRule :: Program -> [Rule] -> Rule
fittingRule pr [] = (\(Env l) PrError sidecons -> prFunc_Skip (Env l))
fittingRule pr (x:xs) 
    | x exampleEnv pr empSideCons == EnvError = fittingRule pr xs
    | otherwise = x
--pl.: (fittingRule Drop initRules) initEnv Drop empSideCons

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

------------------------------------- HELPER FUNCTIONS

helper_SetFieldValidity :: Header -> String -> Validity -> Header
helper_SetFieldValidity (hid, (hv, f)) id v = (hid, (hv, (map (\x@(id', v') -> if id' == id then (id, v) else x) f)))

helper_getValidity :: String -> [Field] -> Validity
helper_getValidity str [] = Undefined
helper_getValidity str (x:xs)
    | fst x == str = snd x
    | otherwise = helper_getValidity str xs


------------------------------------- RULES

initRules :: [Rule]
initRules = [
        (\(Env l) pr sidecons -> case pr of
            Drop -> prFunc_Drop (Env l)
            _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                Skip -> prFunc_Skip (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                SetHeaderValidity (str,v) -> (prFunc_SetHeaderValidity (Env l) str usedValidity)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                SetFieldValidity str v -> (prFunc_SetFieldValidity (Env l) str usedValidity)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                If str pr1 pr2 -> prFunc_If (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                Seq pr1 pr2 -> prFunc_Seq (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                Table str keys prs -> prFunc_Table (Env l)
                _ -> EnvError)
        ]

usedStr :: String
usedStr = "ipv4"

usedValidity :: Validity
usedValidity = Valid

empSideCons :: SideCondition
empSideCons = SideCon []

ifSideCons :: SideCondition
ifSideCons = SideCon ["a"]

exampleEnv :: Environment
exampleEnv = Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Valid),("srcAddr", Valid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]