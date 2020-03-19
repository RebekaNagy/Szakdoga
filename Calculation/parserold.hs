module ParserOld where
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
    | ActAsgn String String
    | Drop 
    | SetHeaderValidity String
    | SetFieldValidity String deriving (Show, Eq)

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

------------------------------------- 'GRESS TO PROGRAM FUNCIONS

strGressToList :: String -> [[String]]
strGressToList gress = filter ((>0) . length) (map (\x -> (words . takeUntil ";" . takeUntil "{") x) (lines gress))

gressCutting :: [[String]] -> [Program] -> Program
gressCutting [] prs = PrError --hogy ne legyen gond a [Program]
gressCutting (x:xs) prs 
    | (x!!0) == "action" = gressCutting xs ((ActCons (takeUntil "(" (x!!1)) (proccessAction (xs))):prs) 
    | (x!!0) == "table" = gressCutting xs ((proccessTable (x:xs) prs):prs) 
    | (x!!0) == "apply" = (proccessApply xs prs True)
    | otherwise = gressCutting xs prs 
-- pl. gressCutting (strGressToList strGress1) initProg

proccessAction :: [[String]] -> Program
proccessAction (x:xs)
    | isInfixOf "mark_to_drop" (x!!0) = Seq Drop (proccessAction xs)
    | (x!!0) == "}" = Skip
    | (isInfixOf "=" (x!!0) || (x!!1) == "=") = Seq (ActAsgn (drop 1 (dropWhile (/= '.') (x!!0))) (drop 1 (dropWhile (/= '=') (x!!0)))) (proccessAction xs)

proccessTable :: [[String]] -> [Program] -> Program
proccessTable (x:y:z:xs) prs
    | ((x!!0) == "table") && (y!!0 == "key") = Table (x!!1) (getKeys (z:xs)) (getActions xs prs)
    | (x!!0) == "table" = Table (x!!1) [] (getActions xs prs)

getKeys :: [[String]] -> [String]
getKeys (x:xs)
    | (x!!0) == "}" = []
    | otherwise = (takeUntil ":" (drop 1 (dropWhile (/= '.') (x!!0)))) : getKeys xs

getActions :: [[String]] -> [Program] -> [Program]
getActions (x:xs) prs
    | (x!!0) == "actions" = helper_getActions xs prs
    | otherwise = getActions xs prs

helper_getActions :: [[String]] -> [Program]-> [Program]
helper_getActions (x:xs) prs
    | (x!!0) == "}" = []
    | otherwise = (filter (\z -> case z of 
                ActCons id pr -> if id == (x!!0) then True else False
                _ -> False) prs) ++ (helper_getActions xs prs)

getTable :: String -> [Program] -> Program
getTable str prs
    | table == [] = PrError
    | otherwise = (table!!0)
    where table = (filter (\z -> case z of 
                Table id keys programs -> if id == str then True else False
                _ -> False) prs)

proccessApply :: [[String]] -> [Program] -> Bool -> Program
proccessApply [] prs False = Skip
proccessApply (x:xs) prs b
    | ((x!!0) == "if" || isInfixOf "if" (x!!0)) && (isThereElse xs == False) = If (getIfString x) (proccessApply xs prs b) Skip
    | ((x!!0) == "if" || isInfixOf "if" (x!!0)) && (isThereElse xs == True) = If (getIfString x) (proccessApply xs prs b) (proccessApply (dropUntilBracket xs) prs False)
    | ((x!!0) == "else" || isInfixOf "else" (x!!0) || ((length x) > 1 && (x!!1) == "else")) && b == True = proccessApply (dropUntilBracket xs) prs b
    | ((x!!0) == "else" || isInfixOf "else" (x!!0) || ((length x) > 1 && (x!!1) == "else")) && b == False = proccessApply (getUntilBracket xs) prs b
    | (isInfixOf ".apply()" (x!!0)) = Seq (getTable (takeUntil ".apply()" (x!!0)) prs) (proccessApply xs prs b)
    | (x!!0) == "control" = Skip
    | otherwise = proccessApply xs prs b


isThereElse :: [[String]] -> Bool
isThereElse [] = False
isThereElse (x:xs)
    | ((x!!0) == "if" || isInfixOf "if" (x!!0)) = False
    | ((x!!0) == "control" || (x!!0) == "V1Switch") = False
    | ((x!!0) == "else" || isInfixOf "else" (x!!0) || ((length x) > 1 && (x!!1) == "else")) = True
    | otherwise = isThereElse xs

getIfString :: [String] -> String
getIfString x
    | length x == 1 = helper_getIfString (takeUntil ")" (drop 1 (dropWhile (/= '(') (x!!0))))
    | otherwise = helper_getIfString (takeUntil ")" (drop 1 (dropWhile (/= '(') (x!!1))))

helper_getIfString :: String -> String
helper_getIfString str
    | isInfixOf ".isValid" str = takeUntil ".isValid" str
    | isInfixOf "==" str = takeUntil "==" str
    | isInfixOf "!=" str = takeUntil "!=" str
    | otherwise = str

dropUntilBracket :: [[String]] -> [[String]]
dropUntilBracket (x:xs) 
    | ((x!!0) == "}" || isInfixOf "}" (x!!0)) = xs
    | otherwise = getUntilBracket xs

getUntilBracket :: [[String]] -> [[String]]
getUntilBracket (x:xs)
    | ((x!!0) == "}" || isInfixOf "}" (x!!0)) = []
    | otherwise = x : getUntilBracket xs

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

strGress1 :: String
strGress1 = "control MyIngress(inout headers hdr,\n\
\        inout metadata meta,\n\
\        inout standard_metadata_t standard_metadata) {\n\
\       \n\
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
\            if (hdr.ipv4 == 2) {\n\
\                ipv4_lpm.apply();\n\
\            }\n\
\        }\n\
\}\n\
\ control"

strGress2 :: String
strGress2 = "control ingressImpl(inout headers_t hdr,\n\
\        inout metadata_t meta,\n\
\        inout standard_metadata_t stdmeta) {\n\
\       \n\
\        action my_drop() {\n\
\            mark_to_drop(stdmeta);\n\
\        }\n\
\       \n\
\        action set_l2ptr(bit<32> l2ptr) {\n\
\            meta.fwd_metadata.l2ptr = l2ptr;\n\
\        }\n\
\        table ipv4_da_lpm {\n\
\            key = {\n\
\                hdr.ipv4.dstAddr: lpm;\n\
\            }\n\
\            actions = {\n\
\                set_l2ptr;\n\
\                my_drop;\n\
\            }\n\
\            default_action = my_drop;\n\
\        }\n\
\       \n\
\        action set_bd_dmac_intf(bit<24> bd, bit<48> dmac, bit<9> intf) {\n\
\            meta.fwd_metadata.out_bd = bd;\n\
\            hdr.ethernet.dstAddr = dmac;\n\
\            stdmeta.egress_spec = intf;\n\
\            hdr.ipv4.ttl = hdr.ipv4.ttl - 1;\n\
\        }\n\
\        table mac_da {\n\
\            key = {\n\
\                meta.fwd_metadata.l2ptr: exact;\n\
\            }\n\
\            actions = {\n\
\                set_bd_dmac_intf;\n\
\                my_drop;\n\
\            }\n\
\            default_action = my_drop;\n\
\        }\n\
\       \n\
\        apply {\n\
\            ipv4_da_lpm.apply();\n\
\            mac_da.apply();\n\
\        }\n\
\}\n\
\ control"

exampleEnv :: Environment
exampleEnv = Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Valid),("srcAddr", Valid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]