module Verification where
import Parser

type Rule = Environment -> Program -> SideCondition -> Environment

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

helper_SetFieldValidity :: Header -> String -> Validity -> Header
helper_SetFieldValidity (hid, (hv, f)) id v = (hid, (hv, (map (\x@(id', v') -> if id' == id then (id, v) else x) f)))

prFunc_If :: Environment -> Environment
prFunc_If (Env l) = (Env l)

prFunc_Seq :: Environment -> Environment
prFunc_Seq (Env l) = (Env l)

prFunc_Table :: Environment -> Environment
prFunc_Table (Env l) = (Env l)

fittingRule :: Program -> [Rule] -> Rule
fittingRule pr [] = (\(Env l) PrError sidecons -> prFunc_Skip (Env l))
fittingRule pr (x:xs) 
    | x initEnv pr empSideCons == EnvError = fittingRule pr xs
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

helper_getValidity :: String -> [Field] -> Validity
helper_getValidity str [] = Undefined
helper_getValidity str (x:xs)
        | fst x == str = snd x
        | otherwise = helper_getValidity str xs

initRules :: [Rule]
initRules = [
        (\(Env l) pr sidecons -> case pr of
                Drop -> prFunc_Drop (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                Skip -> prFunc_Skip (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                SetHeaderValidity str -> (prFunc_SetHeaderValidity (Env l) str usedValidity)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                SetFieldValidity str -> (prFunc_SetFieldValidity (Env l) str usedValidity)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                If pr1 pr2 -> prFunc_If (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                Seq pr1 pr2 -> prFunc_Seq (Env l)
                _ -> EnvError),
        (\(Env l) pr sidecons -> case pr of
                Table str prs -> prFunc_Table (Env l)
                _ -> EnvError)
        ]
