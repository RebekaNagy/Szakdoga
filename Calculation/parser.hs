module Parser where
import Data.String
import Data.List

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
    | If Program Program
    | Table String [Program]
    | Drop 
    | SetHeaderValidity String
    | SetFieldValidity String  deriving Show

data SideCondition = SideCon [String] deriving Show

--stringo :: String -> String
--stringo "a_" = "a"

takeUntil :: String -> String -> String
takeUntil xs [] = [] 
takeUntil [] ys = [] 
takeUntil xs (y:ys) = if   isPrefixOf xs (y:ys)
                      then []
                      else y:(takeUntil xs (tail (y:ys)))

stringToProgram :: String -> String
stringToProgram asdf = asdf

usedStr :: String
usedStr = "ipv4"

usedValidity :: Validity
usedValidity = Valid

empSideCons :: SideCondition
empSideCons = SideCon []

ifSideCons :: SideCondition
ifSideCons = SideCon ["srcAddr"]

initEnv :: Environment
initEnv = Env [
        ("drop", (Invalid, [])), 
        ("ipv4", (Invalid, [("dstAddr", Valid),("srcAddr", Valid)])),
        ("ethernet", (Valid, [("field1", Valid),("field2", Valid)]))
        ]