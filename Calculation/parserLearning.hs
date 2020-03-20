
data BoolExpr = 
    BoolConstant Bool
    | Not BoolExpr
    | BoolBinary BoolBinaryOperator BoolExpr BoolExpr
    | RelationBinary RelationBinaryOperator ArithmeticExpr ArithmeticExpr
    | IsValid ArithmeticExpr
    | IsInvalid ArithmeticExpr
    deriving (Show)

data BoolBinaryOperator = And | Or deriving (Show)

data RelationBinaryOperator = Greater | Less deriving (Show)

data ArithmeticExpr = Variable String
    | IntConstant Integer
    | Negated ArithmeticExpr
    | ArithmeticBinary ArithmeticBinaryOperator ArithmeticExpr ArithmeticExpr
    deriving (Show)

data ArithmeticBinaryOperator = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show)

data Statement = Skip
    | If BoolExpr Statement
    | Else Statement
    | While BoolExpr Statement
    | Seq [Statement]
    | Parameters [Statement]
    | Colons [Statement]
    | Table String Statement
    | Key Statement
    | Actions Statement
    | Action String Statement Statement
    | Assign String ArithmeticExpr
    | Drop
    | SetValid ArithmeticExpr
    | SetInvalid ArithmeticExpr
    | CountFunction ArithmeticExpr
    | ApplyFunction ArithmeticExpr
    | EmitFunction ArithmeticExpr
    | ExtractFunction ArithmeticExpr
    | Parser Statement Statement
    | StateFunction String Statement 
    | Apply Statement 
    | Header String Statement
    | Var ArithmeticExpr
    | VarType String ArithmeticExpr
    | Constant String String ArithmeticExpr
    | Struct String Statement
    | Control Statement Statement
    | UpdateChecksum Statement
    | VerifyChecksum Statement
    | V1Switch Statement Statement
    | Typedef String ArithmeticExpr
    | DirectCounter Statement ArithmeticExpr
    | Hash Statement
    | Include String
    | TransSelect Statement Statement
    | Transition ArithmeticExpr
    deriving (Show)

languageDef =
    emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.reservedNames   = [ "if", "then", "else"
                                        , "while", "do", "}"
                                        , "}", ";", "(", ")"
                                        ,"true", "false"
                                        , "not", "and", "or"
                                        , "actions", "control"
                                        , "action", "apply", "key", "table"
                                        , "direct_counter"
                                        , ".count", "mark_to_drop", ".apply"
                                        , "isInvalid", "isValid"
                                        , "setInvalid", "setValid"
                                        , "hash", "header", "struct", "parser"
                                        , "const", ".extract", ".emit"
                                        , "typedef", "state"
                                        , "#include", "V1Switch"
                                        , "verify_checksum", "update_checksum"
                                        , "transition", "transition select"
                                        ]
            , Token.reservedOpNames = [ "+", "-", "*", "/", "="
                                        , "<", ">", "&&", "||", "!="
                                        , "==", "!" 
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

p4Parser :: Parser [Statement]
p4Parser = do
    whiteSpace
    manyTill statement $ try (eof)

statement :: Parser Statement
statement = parens statement
    <|> braces statement
    <|> statementType
    <|> statementSemis
    <|> statementParameters
    <|> statementColons

statementSemis = do 
    list <- (endBy statementType semi)
    return $ if length list == 1 then head list else Seq (reverse (tail (reverse list)))

statementParameters = do 
    list <- (sepBy1 statementType comma)
    return $ if length list == 1 then head list else Parameters list

statementColons = do 
    list <- (sepBy1 statementType colon)
    return $ if length list == 1 then head list else Colons list

statementType :: Parser Statement
statementType = skipStmt
    <|> whileStmt
    <|> ifStmt
    <|> elseStmt
    <|> dropStmt
    <|> tableStmt
    <|> keyStmt
    <|> actionsStmt
    <|> actionStmt
    <|> parserStmt
    <|> stateFunctionStmt
    <|> headerStmt
    <|> applyFunctionStmt
    <|> varStmt
    <|> varTypeStmt
    <|> constantStmt
    <|> typedefStmt
    <|> setValidStmt
    <|> setInvalidStmt
    <|> countFunctionStmt
    <|> applyStmt
    <|> emitFunctionStmt
    <|> extractFunctionStmt
    <|> assignStmt
    <|> structStmt
    <|> controlStmt
    <|> updateCheckStmt
    <|> verifyCheckStmt
    <|> v1switchStmt
    <|> directCounterStmt
    <|> hashStmt
    <|> includeStmt
    <|> transSelectStmt
    <|> transitionStmt

ifStmt :: Parser Statement
ifStmt = do 
    reserved "if"
    cond  <- bExpression
    stmt <- statement
    return $ If cond stmt

elseStmt :: Parser Statement
elseStmt = do
    reserved "else"
    stmt <- statement
    return $ Else stmt

whileStmt :: Parser Statement
whileStmt = do 
    reserved "while"
    cond <- bExpression
    reserved "do"
    stmt <- statement
    return $ While cond stmt

assignStmt :: Parser Statement
assignStmt = do 
    var  <- identifier
    reservedOp "="
    expr <- aExpression
    return $ Assign var expr

skipStmt :: Parser Statement
skipStmt = reserved "}" >> return Skip

tableStmt :: Parser Statement
tableStmt = do
    reserved "table"
    id <- identifier
    stmt <- statement
    return $ Table id stmt

keyStmt :: Parser Statement
keyStmt = do
    reserved "key"
    reservedOp "="
    stmt <- statement
    return $ Key stmt

actionsStmt :: Parser Statement
actionsStmt = do
    reserved "actions"
    reservedOp "="
    stmt <- statement
    return $ Actions stmt

actionStmt :: Parser Statement
actionStmt = do
    reserved "action"
    id <- identifier
    params <- statement
    stmt <- statement
    return $ Action id params stmt

dropStmt :: Parser Statement
dropStmt = do
    reserved "mark_to_drop"
    id <- identifier
    return $ Drop

setValidStmt :: Parser Statement
setValidStmt = do
    id <- identifier
    reserved "setValid"
    return $ SetValid (Variable id)

setInvalidStmt :: Parser Statement
setInvalidStmt = do
    id <- identifier
    reserved "setInvalid"
    return $ SetInvalid (Variable id)

countFunctionStmt :: Parser Statement
countFunctionStmt = do
    id <- identifier
    reserved ".count"
    return $ CountFunction (Variable id)

applyFunctionStmt :: Parser Statement
applyFunctionStmt = do
    id <- identifier
    reserved ".apply" >> reserved "(" >> reserved ")" >> reserved ";"
    return $ ApplyFunction (Variable id)

emitFunctionStmt :: Parser Statement
emitFunctionStmt = do
    id <- identifier
    reserved ".emit"
    return $ EmitFunction (Variable id)

extractFunctionStmt :: Parser Statement
extractFunctionStmt = do
    id <- identifier
    reserved ".extract"
    return $ ExtractFunction (Variable id)

parserStmt :: Parser Statement
parserStmt = do
    reserved "parser"
    id <- identifier
    params <- statement
    stmt <- statement
    return $ Parser params stmt

stateFunctionStmt :: Parser Statement
stateFunctionStmt = do
    reserved "state"
    id <- identifier
    stmt <- statement
    return $ StateFunction id stmt

applyStmt :: Parser Statement
applyStmt = do
    reserved "apply"
    stmt <- statement
    return $ Apply stmt

headerStmt :: Parser Statement
headerStmt = do
    reserved "header"
    id <- identifier
    stmt <- statement
    return $ Header id stmt

constantStmt :: Parser Statement
constantStmt = do
    reserved "const"
    str1 <- identifier
    str2 <- identifier
    id <- identifier
    return $ Constant str1 str2 (Variable id)

typedefStmt :: Parser Statement
typedefStmt = do
    reserved "typedef"
    str <- identifier
    id <- identifier
    return $ Typedef str (Variable id)

varTypeStmt :: Parser Statement
varTypeStmt = do
    str <- identifier
    id <- identifier
    return $ VarType str (Variable id)

varStmt :: Parser Statement
varStmt = do
    id <- identifier
    return $ Var (Variable id)

structStmt :: Parser Statement
structStmt = do
    reserved "struct"
    id <- identifier
    stmt <- statement
    return $ Struct id stmt

controlStmt :: Parser Statement
controlStmt = do
    reserved "control"
    params <- statement
    stmt <- statement
    return $ Control params stmt

updateCheckStmt :: Parser Statement
updateCheckStmt = do
    reserved "update_checksum"
    stmt <- statement
    return $ UpdateChecksum stmt

verifyCheckStmt :: Parser Statement
verifyCheckStmt = do
    reserved "verify_checksum"
    stmt <- statement
    return $ VerifyChecksum stmt

v1switchStmt :: Parser Statement
v1switchStmt = do
    reserved "V1Switch"
    stmt1 <- statement
    stmt2 <- statement
    return $ V1Switch stmt1 stmt2

directCounterStmt :: Parser Statement
directCounterStmt = do
    reserved "direct_counter"
    stmt <- statement
    id <- identifier
    return $ DirectCounter stmt (Variable id)

hashStmt :: Parser Statement
hashStmt = do
    reserved "hash"
    stmt <- statement
    return $ Hash stmt

includeStmt :: Parser Statement
includeStmt = do
    reserved "#include"
    id <- identifier
    return $ Include id

transSelectStmt :: Parser Statement
transSelectStmt = do
    reserved "transition select"
    params <- statement
    stmt <- statement
    return $ TransSelect params stmt

transitionStmt :: Parser Statement
transitionStmt = do
    reserved "transition"
    id <- identifier
    return $ Transition (Variable id)

---------------------------------------------------------
aExpression :: Parser ArithmeticExpr
aExpression = buildExpressionParser aOperators aTerm
        <|> aTerm

bExpression :: Parser BoolExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Negated             ))          ]
             , [Infix  (reservedOp "*"   >> return (ArithmeticBinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ArithmeticBinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ArithmeticBinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ArithmeticBinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "!" >> return (Not             ))          ]
             , [Infix  (reservedOp "&&" >> return (BoolBinary And     )) AssocLeft,
                Infix  (reservedOp "||"  >> return (BoolBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
    <|> liftM Variable identifier
    <|> liftM IntConstant integer

bTerm =  parens bExpression
    <|> (reserved "true"  >> return (BoolConstant True ))
    <|> (reserved "false" >> return (BoolConstant False))
    <|> rExpression

rExpression = do 
    a1 <- aExpression
    op <- relation
    a2 <- aExpression
    return $ RelationBinary op a1 a2

relation = (reservedOp ">" >> return Greater)
    <|> (reservedOp "<" >> return Less)

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
