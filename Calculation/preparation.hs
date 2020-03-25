module Preparation where
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

------------------------------------- VARIABLES

initEnv :: Environment
initEnv = Env [("drop",(Invalid, []))]

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

exampleEnv :: Environment
exampleEnv = Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Valid),("srcAddr", Valid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]