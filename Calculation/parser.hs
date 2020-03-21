module Parser where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Statement = Skip
    | Variable String
    | Header String [Variable] 
    | Struct String [Variable] 
    | Parser Statement 
    | State String Statement 
    | Transition String
    | TransitionSelect String [Variable] 
    | StatementFunctionExpr FunctionExpression deriving Show 

data Variable = Field String
    | StructField String
    | Var String 
    | ParameterVar String
    | Select String String deriving Show

data FunctionOperator = Extract
    | Emit 
    | Count
    | Apply deriving Show

data FunctionExpression = Var1 String
    | Var2 String String
    | Var3 String String String
    | FunctionBin FunctionOperator FunctionExpression FunctionExpression deriving Show

languageDef =
    emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.reservedNames   = [ "if", "then", "else"
                                        , "while", "do", "}"
                                        , "}", ";", "(", ")", ","
                                        ,"true", "false"
                                        , "not", "and", "or"
                                        , "actions", "control"
                                        , "action", "apply", "key", "table"
                                        , "direct_counter"
                                        ,  "mark_to_drop"
                                        , "isInvalid", "isValid"
                                        , "setInvalid", "setValid"
                                        , "hash", "header", "struct", "parser"
                                        , "const"
                                        , "typedef", "state"
                                        , "#include", "V1Switch"
                                        , "verify_checksum", "update_checksum"
                                        , "transition select", "transition"
                                        ]
            , Token.reservedOpNames = [ "+", "-", "*", "/", "="
                                        , "<", ">", "&&", "||", "!="
                                        , "==", "!", "count", "extract", "emit", "apply"
                                        ]
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum <|> char '.' <|> char '_' <|> char '<' <|> char '>'
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
colon      = Token.colon      lexer
dot        = Token.dot        lexer
whiteSpace = Token.whiteSpace lexer
charLiteral = Token.charLiteral lexer

p4Parser :: Parser [Statement]
p4Parser = do
    whiteSpace
    manyTill sectionParser $ try (eof)

sectionParser :: Parser Statement
sectionParser = headerSection
    <|> structSection
    <|> parserSection

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
    reserved "("
    parameters <- manyTill parameterParser $ try (reserved ")")
    reserved "{"
    block <- parserComponents
    reserved "}"
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
    value <- manyTill charLiteral $ try (reserved ";")
    return Skip

stateParser :: Parser Statement
stateParser = do
    reserved "state"
    stateName <- identifier
    reserved "{"
    block <- stateComponents
    reserved "}"
    return $ Parser.State stateName block

stateComponents = transitionSelectParser
    <|> transitionParser 
    <|> statementFuncExpr

transitionParser :: Parser Statement
transitionParser = do
    reserved "transition"
    var <- identifier
    return $ Transition var

transitionSelectParser :: Parser Statement
transitionSelectParser = do
    reserved "transition select"
    reserved "("
    param <- identifier
    reserved ")"
    reserved "{"
    selected <- manyTill semicolonParser $ try (reserved "}")
    return $ TransitionSelect param selected

statementFuncExpr :: Parser Statement
statementFuncExpr = do
    expr <- functionExpr
    return $ StatementFunctionExpr expr

------------------------------------- MISC PARSER FUNCTIONS
semicolonParser :: Parser Variable
semicolonParser = do
    var1 <- identifier
    reserved ":"
    var2 <- identifier
    reserved ";"
    return $ Select var1 var2

parameterParser :: Parser Variable
parameterParser = do
    parameterType <- identifier
    paremeterName <- identifier
    reserved ","
    return $ ParameterVar paremeterName

functionExpr :: Parser FunctionExpression
functionExpr = buildExpressionParser functionOperator functionTerm

functionOperator = [
    [Infix (reservedOp "extract" >> return (FunctionBin Extract)) AssocLeft,
    Infix  (reservedOp "emit"  >> return (FunctionBin Emit)) AssocLeft,
    Infix  (reservedOp "count"  >> return (FunctionBin Count)) AssocLeft,
    Infix  (reservedOp "apply"  >> return (FunctionBin Apply)) AssocLeft]]

functionTerm = liftM3 Var3 (reserved "(") identifier (reserved ")")
    <|> liftM2 Var2 (reserved "(") (reserved ")")
    <|> liftM Var1 identifier

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

-- test: parseFile "exam.txt"