module Parser where
import System.IO
import Data.List
import Data.String
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Statement = Skip
    | Seq [Statement]
    | Variable String
    | Header String [Variable] 
    | Struct String [Variable] 
    | Parser [Statement] 
    | State String Statement 
    | Transition String
    | TransitionSelect String [Variable] 
    | FuncExpr FunctionExpression 
    | Control String [Statement]
    | DirectCount String 
    | Action String Statement
    | Assignment String String 
    | Drop 
    | Table Statement Statement
    | Keys [Variable]
    | Acts [String] 
    | Apply Statement 
    | If Statement Statement 
    | VerifyChecksum 
    | UpdateChecksum 
    | V1Switch [String] deriving Show 

data Variable = Field String
    | StructField String
    | VarElements [String]
    | ParameterVar Variable
    | Semi String String deriving Show

data FunctionOperator = Extract
    | Emit 
    | Count
    | ApplyFunc deriving Show

data FunctionExpression = Var String
    | UF FunctionOperator FunctionExpression
    | BF FunctionOperator FunctionExpression FunctionExpression deriving Show

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
                                        ]
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum <|> char '_' <|> char '<' <|> char '>' <|> char '.'
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

------------------------------------- HEADER PARSER FUNCTIONS  
headerSection :: Parser Statement
headerSection = do   
    reserved "header"
    headerName <- identifier
    reserved "{" 
    fields <- manyTill (headerFieldParser headerName) $ try (reserved "}")
    return $ Header headerName fields

headerFieldParser :: String -> Parser Variable
headerFieldParser headerName = do
    fieldType <- identifier
    fieldName <- identifier
    reserved ";"
    return $ Field (headerName ++ "." ++ fieldName) 

------------------------------------- STRUCT PARSER FUNCTIONS
structSection :: Parser Statement
structSection = do
    reserved "struct"
    structName <- identifier
    reserved "{"
    fields <- manyTill structFieldParser $ try (reserved "}")
    return $ Struct structName fields

structFieldParser :: Parser Variable
structFieldParser = do
    fieldType <- identifier
    fieldName <- identifier
    reserved ";"
    return $ StructField fieldName

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
parserComponents = constantAssignmentParser
    <|> stateParser

constantAssignmentParser :: Parser Statement
constantAssignmentParser = do
    reserved "const"
    constType <- identifier
    constName <- identifier
    reservedOp "="
    value <- manyTill anyChar $ try (reserved ";")
    return Skip

stateParser :: Parser Statement
stateParser = do
    reserved "state"
    stateName <- identifier
    reserved "{"
    block <- manyTill stateComponents $ try (reserved "}")
    return $ Parser.State stateName (Seq block)

stateComponents = transitionSelectParser
    <|> transitionParser 
    <|> functionExpr

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
    return $ Control parserName block

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
    return $ Action name (Seq block)

actionComponents :: Parser Statement
actionComponents = dropParser
--    <|> assignmentParser
    <|> functionExpr

dropParser :: Parser Statement
dropParser = do
    reserved "mark_to_drop"
    param <- parens parameterParser
    reserved ";"
    return $ Drop

tableParser :: Parser Statement
tableParser = do
    reserved "table"
    tableName <- identifier
    reserved "{"
    reserved "key"
    reservedOp "="
    reserved "{"
    keys <- manyTill semicolonParser $ try (reserved "}") 
    reserved "actions"
    reservedOp "="
    reserved "{"
    actions <- manyTill actsParser $ try (reserved "}")
--    assigns <- manyTill assignmentParser $ try (reserved "}")
    reserved "}"
    return $ Table (Keys keys) (Acts actions)

actsParser :: Parser String
actsParser = do
    action <- identifier
    reserved ";"
    return action 

applyParser :: Parser Statement
applyParser = do
    reserved "apply"
    reserved "{"
    block <- manyTill applyComponents $ try (reserved "}")
    return $ Apply (Seq block)

applyComponents :: Parser Statement
applyComponents = ifParser 
    <|> verifychecksumParser
    <|> updatechecksumParser
    <|> functionExpr

ifParser :: Parser Statement
ifParser = do 
    reserved "if"
    params <- parens parameterParser
    reserved "{"
    block <- manyTill applyComponents $ try (reserved "}")
    elseBlock <- option Skip elseParser
    return $ If (Seq block) elseBlock

elseParser :: Parser Statement
elseParser = do
    reserved "else"
    reserved "{"
    block <- manyTill applyComponents $ try (reserved "}")
    return $ Seq block

verifychecksumParser :: Parser Statement
verifychecksumParser = do
    reserved "verify_checksum"
    block <- tokenEater
    reserved ";"
    return VerifyChecksum

updatechecksumParser :: Parser Statement
updatechecksumParser = do
    reserved "update_checksum"
    block <- tokenEater
    reserved ";"
    return UpdateChecksum

tokenEater :: Parser String
tokenEater = do
    str <- manyTill anyToken  $ lookAhead (char ';')
    return str

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

functionExpr :: Parser Statement
functionExpr = do
    expr <- buildFunctionExpr
    reserved ";"
    return $ FuncExpr expr

buildFunctionExpr :: Parser FunctionExpression
buildFunctionExpr = buildExpressionParser functionOperator functionTerm

functionOperator = [
    [Infix (reservedOp ".extract" >> return (BF Extract)) AssocLeft,
    Infix  (reservedOp ".emit"  >> return (BF Emit)) AssocLeft,
    Postfix  (reservedOp ".count()"  >> return (UF Count)),
    Postfix  (reservedOp ".apply()"  >> return (UF ApplyFunc)) ]]

functionTerm = liftM Var (parens identifier)
    <|> liftM Var (manyTill anyChar (lookAhead (char '.')))

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

-- test: parseFile "file.txt"