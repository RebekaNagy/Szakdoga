module Parser where
import System.IO
import Data.List
import Data.String
import Data.Char
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Statement = Error
    | ParserSkip
    | ParserSeq [Statement]
    | ParserHeader String [Variable] 
    | ParserStruct String [Variable] 
    | Parser [Statement] 
    | State String Statement 
    | Transition String
    | TransitionSelect String [Variable] 
    | Control String [Statement]
    | DirectCount String 
    | ParserAction String Statement
    | ParserAssignment String ArithmeticExpression 
    | ParserDrop
    | Hash
    | ParserTable String Statement Statement
    | Keys [Variable]
    | Acts [String] 
    | Apply Statement 
    | ParserIf Statement Statement Statement 
    | VerifyChecksum 
    | UpdateChecksum 
    | V1Switch [String] 
    | FuncExpr FunctionExpression 
    | BoolExpr BoolExpression
    | Include String
    | Typedef
    deriving Show 

data Variable = ParserField (String, String)
    | StructField (String, String)
    | VarElements [String]
    | ParameterVar Variable
    | Semi String String 
    | Var String deriving Show

data FunctionExpression = FuncVar String
    | Count FunctionExpression
    | ApplyFunc FunctionExpression
    | SetValid FunctionExpression
    | SetInvalid FunctionExpression
    | Emit FunctionExpression FunctionExpression 
    | Extract FunctionExpression FunctionExpression deriving Show

data ArithmeticExpression = ArithVar String
    | NumConstant String
    | Negated ArithmeticExpression
    | Add ArithmeticExpression ArithmeticExpression
    | Subtract ArithmeticExpression ArithmeticExpression
    | Multiply ArithmeticExpression ArithmeticExpression
    | Divide ArithmeticExpression ArithmeticExpression deriving Show

data BoolExpression = BoolVar String
    | BoolConstant Bool
    | IsValid BoolExpression
    | Not BoolExpression
    | And BoolExpression BoolExpression
    | Or BoolExpression BoolExpression
    | Equal BoolExpression BoolExpression
    | Inequal BoolExpression BoolExpression 
    | Greater BoolExpression BoolExpression
    | Less BoolExpression BoolExpression deriving Show

languageDef =
    emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.reservedNames   = [ "if", "then", "else"
                                        , "while", "do", "}", "."
                                        , "{", ";", "(", ")", ","
                                        ,"true", "false"
                                        , "not", "and", "or"
                                        , "actions", "control"
                                        , "action", "apply", "key", "table"
                                        , "direct_counter"
                                        , "mark_to_drop"
                                        , "isInvalid", "isValid"
                                        , "setInvalid", "setValid"
                                        , "hash", "header", "struct", "parser"
                                        , "const"
                                        , "typedef", "state"
                                        , "#include", "V1Switch"
                                        , "verify_checksum", "update_checksum"
                                        , "transition select", "transition", "main"
                                        ]
            , Token.reservedOpNames = [ "+", "-", "*", "/", "="
                                        , "<", ">", "&&", "||", "!="
                                        , "==", "!", ".count()", ".extract", ".emit", ".apply()"
                                        , ".setValid()", ".setInvalid()"
                                        , ".isValid()"
                                        ]
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum <|> char '_' <|> char '<' <|> char '>'
                                    <|> char '.' <|> char '[' <|> char ']'
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer 
reserved   = Token.reserved   lexer 
reservedOp = Token.reservedOp lexer 
parens     = Token.parens     lexer
braces     = Token.braces     lexer
angles     = Token.angles     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
semiSep    = Token.semiSep    lexer
comma      = Token.comma      lexer
commaSep   = Token.commaSep   lexer
colon      = Token.colon      lexer
dot        = Token.dot        lexer
whiteSpace = Token.whiteSpace lexer
charLiteral = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer
symbol = Token.symbol lexer

p4Parser :: Parser [Statement]
p4Parser = do
    whiteSpace
    manyTill sectionParser $ try (eof)

sectionParser :: Parser Statement
sectionParser = headerSection
    <|> structSection
    <|> parserSection
    <|> controlSection
    <|> v1SwitchSection
    <|> includeSection
    <|> typedefSection
    <|> constantSection
    <|> tokenEater
------------------------------------- HEADER PARSER FUNCTIONS  
headerSection :: Parser Statement
headerSection = do   
    reserved "header"
    headerName <- identifier
    reserved "{" 
    fields <- manyTill (headerFieldParser headerName) $ try (reserved "}")
    return $ ParserHeader headerName fields

headerFieldParser :: String -> Parser Variable
headerFieldParser headerName = do
    fieldType <- identifier
    fieldName <- identifier
    reserved ";"
    return $ ParserField (fieldType, fieldName)

------------------------------------- STRUCT PARSER FUNCTIONS
structSection :: Parser Statement
structSection = do
    reserved "struct"
    structName <- identifier
    reserved "{"
    fields <- manyTill structFieldParser $ try (reserved "}")
    return $ ParserStruct structName fields

structFieldParser :: Parser Variable
structFieldParser = do
    fieldType <- identifier
    fieldName <- identifier
    reserved ";"
    return $ StructField (fieldType, fieldName)

------------------------------------- PARSER PARSER FUNCTIONS
parserSection :: Parser Statement
parserSection = do
    reserved "parser"
    parserName <- identifier
    parameters <- parens parameterParser
    reserved "{"
    block <- manyTill parserComponents $ try (reserved "}")
    return $ Parser block

parserComponents :: Parser Statement
parserComponents = stateParser
    <|> actionExpr

stateParser :: Parser Statement
stateParser = do
    reserved "state"
    stateName <- identifier
    reserved "{"
    block <- manyTill stateComponents $ try (reserved "}")
    return $ Parser.State stateName (ParserSeq block)

stateComponents = transitionSelectParser
    <|> transitionParser 
    <|> actionExpr

transitionParser :: Parser Statement
transitionParser = do
    reserved "transition"
    var <- identifier
    reserved ";"
    return $ Transition var

transitionSelectParser :: Parser Statement
transitionSelectParser = do
    reserved "transition select"
    param <- parens identifier
    reserved "{"
    selected <- manyTill semicolonParser $ try (reserved "}")
    return $ TransitionSelect param selected

------------------------------------- CONTROL PARSER FUNCTIONS
controlSection :: Parser Statement
controlSection = do
    reserved "control"
    parserName <- identifier
    parameters <- parens parameterParser
    reserved "{"
    block <- manyTill controlComponents $ try (reserved "}")
    return $ Parser.Control parserName block

controlComponents :: Parser Statement
controlComponents = direct_counterParser
    <|> actionParser
    <|> tableParser
    <|> applyParser

direct_counterParser :: Parser Statement
direct_counterParser = do
    reserved "direct_counter"
    param <- parens parameterParser
    var <- identifier
    reserved ";"
    return $ DirectCount var

actionParser :: Parser Statement
actionParser = do
    reserved "action"
    name <- identifier
    param <- parens parameterParser
    reserved "{"
    block <- manyTill actionComponents $ try (reserved "}")
    return $ ParserAction name (ParserSeq block)

actionComponents :: Parser Statement
actionComponents = dropParser
    <|> hashParser
    <|> actionExpr

dropParser :: Parser Statement
dropParser = do
    reserved "mark_to_drop"
    param <- parens parameterParser
    reserved ";"
    return $ ParserDrop

hashParser :: Parser Statement
hashParser = do
    reserved "hash"
    block <- manyTill tokenEater $ lookAhead (char ';')
    reserved ";"
    return $ Hash

tableParser :: Parser Statement
tableParser = do
    reserved "table"
    tableName <- identifier
    reserved "{"
    keys <- option (Keys [Var "none"]) keysParser
    reserved "actions"
    reservedOp "="
    reserved "{"
    actions <- manyTill actsParser $ try (reserved "}")
    assigns <- manyTill actionExpr $ try (reserved "}")
    return $ ParserTable tableName (keys) (Acts actions)

keysParser :: Parser Statement
keysParser = do
    reserved "key"
    reservedOp "="
    reserved "{"
    keys <- manyTill semicolonParser $ try (reserved "}")
    return $ Keys keys

actsParser :: Parser String
actsParser = do
    atparse <- option ParserSkip atParser
    action <- identifier
    reserved ";"
    return action

atParser :: Parser Statement
atParser = do
    symbol "@"
    id <- identifier
    return ParserSkip

applyParser :: Parser Statement
applyParser = do
    reserved "apply"
    reserved "{"
    block <- manyTill applyComponents $ try (reserved "}")
    return $ Apply (ParserSeq block)

applyComponents :: Parser Statement
applyComponents = ifParser 
    <|> verifychecksumParser
    <|> updatechecksumParser
    <|> actionExpr

ifParser :: Parser Statement
ifParser = do 
    reserved "if"
    cond <- parens boolExpr
    reserved "{"
    block <- manyTill applyComponents $ try (reserved "}")
    elseBlock <- option ParserSkip elseParser
    return $ ParserIf cond (ParserSeq block) elseBlock

elseParser :: Parser Statement
elseParser = do
    reserved "else"
    reserved "{"
    block <- manyTill applyComponents $ try (reserved "}")
    return $ ParserSeq block

verifychecksumParser :: Parser Statement
verifychecksumParser = do
    reserved "verify_checksum"
    block <- manyTill tokenEater $ lookAhead (char ';')
    reserved ";"
    return VerifyChecksum

updatechecksumParser :: Parser Statement
updatechecksumParser = do
    reserved "update_checksum"
    block <- manyTill tokenEater $ lookAhead (char ';')
    reserved ";"
    return UpdateChecksum

tokenEater :: Parser Statement
tokenEater = do
    str <- anyToken
    return Error

------------------------------------- V1SWITCH PARSER FUNCTIONS
v1SwitchSection :: Parser Statement
v1SwitchSection = do
    reserved "V1Switch"
    block <- parens funcParameterList
    reserved "main"
    reserved ";"
    return $ V1Switch block

funcParameterList :: Parser [String]
funcParameterList = do
    str1 <- identifier
    reserved "("
    reserved ")"
    strlist <- manyTill funcParameterParser $ lookAhead (char ')')
    return (str1 : strlist) 

funcParameterParser :: Parser String
funcParameterParser = do
    reserved ","
    funcparam <- identifier
    reserved "("
    reserved ")"
    return funcparam

------------------------------------- INCLUDE PARSER FUNCTIONS
includeSection :: Parser Statement
includeSection = do
    reserved "#include"
    reservedOp "<"
    id <- manyTill anyChar $ lookAhead (char '>')
    reserved ">"
    return $ Include id

------------------------------------- TYPEDEF PARSER FUNCTIONS
typedefSection :: Parser Statement
typedefSection = do
    reserved "typedef"
    typedef <- manyTill anyToken  $ lookAhead (char ';')
    reserved ";"
    return $ Typedef

------------------------------------- CONSTANT PARSER FUNCTIONS
constantSection :: Parser Statement
constantSection = do
    reserved "const"
    expr <- actionExpr
    return $ expr

------------------------------------- MISC PARSER FUNCTIONS
identifierParser :: Parser Variable
identifierParser = do
    str1 <- option "" identifier
    str2 <- option "" identifier
    str3 <- option "" identifier
    return $ VarElements [str1, str2, str3]

semicolonParser :: Parser Variable
semicolonParser = do
    var1 <- identifier
    reserved ":"
    var2 <- identifier
    reserved ";"
    return $ Semi var1 var2

parameterParser :: Parser Variable
parameterParser = do
    id <- identifierParser
    list <- manyTill ((reserved ",") >> identifierParser) $ lookAhead (char ')')
    return $ ParameterVar id

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
------------------------------------- FUNCTION EXPRESSION PARSER FUNCTIONS
actionExpr :: Parser Statement
actionExpr = do
    var <- (manyTill anyChar (lookAhead (reservedOp ".extract"
                                                <|> reservedOp ".emit"
                                                <|> reservedOp ".count()"
                                                <|> reservedOp ".apply()"
                                                <|> reservedOp ".setValid()"
                                                <|> reservedOp ".setInvalid()"
                                                <|> reservedOp "=")))
    isassign <- option "" (symbol "=")
    case isassign of
        "" -> do
            expr <- buildFunctionExpr var
            reserved ";"
            return $ FuncExpr expr
        _ -> do
            expr <- buildArithmeticExpr
            reserved ";"
            return $ ParserAssignment (trim var) expr

buildFunctionExpr :: String -> Parser FunctionExpression
buildFunctionExpr str = buildExpressionParser functionOperator (functionTerm str)

functionOperator = [
    [Infix (reservedOp ".extract" >> return (Extract)) AssocLeft,
    Infix  (reservedOp ".emit"  >> return (Emit)) AssocLeft,
    Postfix  (reservedOp ".count()"  >> return (Count)),
    Postfix  (reservedOp ".apply()"  >> return (ApplyFunc)),
    Postfix  (reservedOp ".setValid()"  >> return (SetValid)),
    Postfix  (reservedOp ".setInvalid()"  >> return (SetInvalid)) ]]

functionTerm :: String -> Parser FunctionExpression
functionTerm str = liftM FuncVar (parens identifier)
    <|> liftM FuncVar (option str (string "nothing"))

------------------------------------- ARITHMETIC EXPRESSION PARSER FUNCTIONS

buildArithmeticExpr :: Parser ArithmeticExpression
buildArithmeticExpr = buildExpressionParser arithmeticOperator arithmeticTerm

arithmeticOperator = [ 
    [Prefix (reservedOp "-" >> return (Negated))],
    [Infix  (reservedOp "*" >> return (Multiply)) AssocLeft,
    Infix  (reservedOp "/" >> return (Divide)) AssocLeft],
    [Infix  (reservedOp "+" >> return (Add)) AssocLeft,
    Infix  (reservedOp "-" >> return (Subtract)) AssocLeft]]

arithmeticTerm = parens buildArithmeticExpr
    <|> liftM ArithVar (do 
        id <- identifier 
        o <- (option "" (string "()"))
        spaces
        return id)
    <|> liftM NumConstant (many (digit <|> letter))

------------------------------------- BOOLEAN EXPRESSION PARSER FUNCTIONS
boolExpr :: Parser Statement
boolExpr = do
    expr <- buildBoolExpr
    return $ BoolExpr expr

buildBoolExpr :: Parser BoolExpression
buildBoolExpr = buildExpressionParser boolOperator boolTerm

boolOperator = [ 
    [Prefix (reservedOp "!" >> return (Not)),
    Postfix (reservedOp ".isValid()" >> return IsValid),
    Infix (reservedOp ">" >> return (Greater)) AssocLeft,
    Infix (reservedOp "<" >> return (Less)) AssocLeft,
    Infix (reservedOp ">=" >> return (Greater)) AssocLeft,
    Infix (reservedOp "<=" >> return (Less)) AssocLeft,
    Infix (reservedOp "==" >> return (Equal)) AssocLeft,
    Infix (reservedOp "!=" >> return (Inequal)) AssocLeft],
    [Infix  (reservedOp "&&" >> return (And)) AssocLeft,
    Infix  (reservedOp "||"  >> return (Or)) AssocLeft]]

boolTerm = parens buildBoolExpr
    <|> (reserved "true"  >> return (BoolConstant True ))
    <|> (reserved "false" >> return (BoolConstant False))
    <|> liftM BoolVar (do 
        var <- (manyTill anyChar (lookAhead (reservedOp ".isValid()"
                                            <|> reservedOp "||"
                                            <|> reservedOp "&&"
                                            <|> reservedOp "!="
                                            <|> reservedOp "!"
                                            <|> reservedOp "=="
                                            <|> reservedOp "<="
                                            <|> reservedOp "<"
                                            <|> reservedOp ">="
                                            <|> reservedOp ">"
                                            <|> reservedOp ")"
                                            )))
        return (trim var))

------------------------------------- FUNCTIONS TO RUN THE PARSER

parseString :: String -> [Statement]
parseString str = case parse p4Parser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO [Statement]
parseFile file = do 
    program  <- readFile file
    case parse p4Parser "" program of
        Left e  -> print e >> fail "parse error"
        Right r -> return r

-- test -> :main

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
\            if (hdr.ipv4 > 2) {\n\
\                ipv4_lpm.apply();\n\
\            }\n\
\        }\n\
\}"