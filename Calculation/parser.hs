module Parser where
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
    PrError
    | Skip
    | Seq Program Program
    | If String Program Program
    | Table String [String] [Program]
    | ActCons String Program
    | ActAsgn String Integer
    | Drop 
    | SetHeaderValidity String
    | SetFieldValidity String deriving Show

data SideCondition = SideCon [String] deriving Show

------------------------------------- MISC FUNCTIONS

takeUntil :: String -> String -> String
takeUntil s [] = [] 
takeUntil [] ys = [] 
takeUntil s (y:ys) 
    | isPrefixOf s (y:ys) = []
    | otherwise = y : (takeUntil s (tail (y:ys)))

strToProg :: String -> String
strToProg asdf = asdf

------------------------------------- HEADER TO ENVIRONMENT FUNCTIONS

strHeaderToList :: String -> [[String]]
strHeaderToList headers = filter ((>1) . length) (map (\x -> (words . takeUntil ";") x) (lines headers))
-- pl. strHeaderToList strHeader

listToHeader :: [[String]] -> Header
listToHeader (x:xs) = (hid, (Invalid, (listToField xs hid)))
    where hid = x!!1
-- pl. listToHeader (strHeaderToList strHeader)

listToField :: [[String]] -> String -> [Field]
listToField [] hid = []
listToField (x:xs) hid = ((hid ++ "." ++ (x!!1)), Valid) : (listToField xs hid)

concatHeaders :: Environment -> Header -> Environment
concatHeaders (Env l) header = Env (l ++ [header])
-- pl. concatHeaders (concatHeaders initEnv (listToHeader (strHeaderToList strHeader1))) (listToHeader (strHeaderToList strHeader2))

------------------------------------- CONTROL TO PROGRAM FUNCIONS

strControlToList :: String -> [[String]]
strControlToList control = filter ((>0) . length) (map (\x -> (words . takeUntil ";" . takeUntil "{") x) (lines control))

controlCutting :: [[String]] -> [Program] -> Program
controlCutting [] pr = Table "a" ["a"] pr --hogy ne legyen gond a [Program]
controlCutting (x:xs) pr
    | x!!0 == "action" = controlCutting xs ((ActCons (takeUntil "()" (x!!1)) (proccessAction (xs))):pr)
    | x!!0 == "table" = controlCutting xs ((proccessTable (x:xs) pr):pr)
    | x!!0 == "apply" = controlCutting xs pr
    | otherwise = controlCutting xs pr
--controlCutting (strControlToList strControl) initProg

proccessAction :: [[String]] -> Program
proccessAction (x:xs)
--    | x!!0 == "action" = ActCons (x!!1) (proccessAction xs)
    | isInfixOf "mark_to_drop" (x!!0) = Seq Drop (proccessAction xs)
    | x!!0 == "}" = Skip
    | isInfixOf "=" (x!!1) = Seq (ActAsgn (drop 1 (dropWhile (/= '.') (x!!0))) (read (x!!2))) (proccessAction xs)

proccessTable :: [[String]] -> [Program] -> Program
proccessTable (x:y:z:xs) prs
    | (x!!0 == "table") && (y!!0 == "key") = Table (x!!1) (getKeys (z:xs)) (getActions xs prs)
    | x!!0 == "table" = Table (x!!1) [] (getActions xs prs)
--    | x!!0 == "actions" = getActions xs

getKeys :: [[String]] -> [String]
getKeys (x:xs)
    | x!!0 == "}" = []
    | otherwise = (drop 1 (dropWhile (/= '.') (x!!0))) : getKeys xs

getActions :: [[String]] -> [Program] -> [Program]
getActions (x:xs) prs
    | x!!0 == "actions" = helper_getActions xs prs
    | otherwise = getActions xs prs

helper_getActions :: [[String]] -> [Program]-> [Program]
helper_getActions (x:xs) prs
    | x!!0 == "}" = []
    | otherwise = (filter (\z -> case z of 
                ActCons id pr -> if id == x!!0 then True else False
                _ -> False) prs) ++ (helper_getActions xs prs)

------------------------------------- VARIABLES

initEnv :: Environment
initEnv = Env [("drop",(Invalid, []))]

strHeader1 :: String
strHeader1 = "header ethernet_t {\n\
\    bit<48> dstAddr;\n\  
\    bit<48> srcAddr;\n\  
\    bit<16> etherType;\n\
\}"

strHeader2 :: String
strHeader2 = "header ipv4_t {\n\
\    bit<8> ttl;\n\
\    bit<16> hdrChecksum;\n\
\    bit<32> srcAddr;\n\
\    bit<32> dstAddr;\n\
\}"

initProg :: [Program]
initProg = []

usedStr :: String
usedStr = "ipv4"

usedValidity :: Validity
usedValidity = Valid

empSideCons :: SideCondition
empSideCons = SideCon []

ifSideCons :: SideCondition
ifSideCons = SideCon ["a"]

strActs :: String
strActs = "action ipv4_ch() {\n\
\   hdr.ethernet.srcAddr = 2;\n\
\   hdr.ethernet.dstAddr = 1;\n\
\   hdr.ipv4.ttl = 20;\n\
\}"

strControl :: String
strControl = "control MyIngress(inout headers hdr,\n\
\        inout metadata meta,\n\
\        inout standard_metadata_t standard_metadata) {\n\
\    \n\
\        action drop() {\n\
\            mark_to_drop(standard_metadata);\n\
\        }\n\
\         \n\
\        action ipv4_ch() {\n\
\            hdr.ethernet.srcAddr = 2;\n\
\            hdr.ethernet.dstAddr = 1;\n\
\            hdr.ipv4.ttl = 20;\n\
\        }\n\
\        \n\
\        table ipv4_lpm {\n\
\            key = {\n\
\                hdr.ipv4.dstAddr: lpm;\n\
\            }\n\
\            actions = {\n\
\                ipv4_ch;\n\
\                drop;\n\
\            }\n\
\            size = 1024;\n\
\            default_action = drop();\n\
\        }\n\
\        \n\
\        apply {\n\
\            if (hdr.ipv4.isValid()) {\n\
\                ipv4_lpm.apply();\n\
\            }\n\
\        }\n\
\}"

exampleEnv :: Environment
exampleEnv = Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Valid),("srcAddr", Valid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]