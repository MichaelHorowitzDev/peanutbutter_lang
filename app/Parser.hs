module Parser (
 parseProgram
) where
    
--import qualified Lexer as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad
import Ast
    
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = (space >>)
    
lexeme1 :: Parser a -> Parser a
lexeme1 = (space1 >>)

floatNum :: Parser Float
floatNum = do
    int <- some digitChar
    (do 
        char '.'
        float <- some digitChar
        return $ read $ int ++ "." ++ float)
     <|> (return $ read int)
     
parseFloat :: Parser Float
parseFloat = (char '-' >> negate <$> floatNum) <|> floatNum

parseString :: Parser String
parseString = char '\"' *> manyTill L.charLiteral (char '\"')

parseIdentifier :: Parser String
parseIdentifier = some letterChar

parseBool :: Parser Bool
parseBool = (string "true" >> return True) <|> (string "false" >> return False)

parsePrimary :: Parser Exp
parsePrimary = space >> (parseString >>= return . Lit . String) 
    <|> (parseBool >>= return . Lit . Bool) 
    <|> (parseFloat >>= return . Lit . Float) 
    <|> (parseIdentifier >>= return . Var)
    <|> (do
        char '('
        expr <- parseExpression
        char ')'
        return expr)
        
parseUnary :: Parser Exp
parseUnary = space >> (do
    op <- char '-' <|> char '!'
    u <- parseUnary
    case op of
        '-' -> return (Negate u)
        '!' -> return (Bang u))
    <|> parsePrimary

parseFactor :: Parser Exp
parseFactor = do
    space
    u <- parseUnary
    space
    flattenedFactor u
     where
         flattenedFactor :: Exp -> Parser Exp
         flattenedFactor p = (do
             op <- try $ lexeme (char '*' <|> char '/')
             p1 <- parseUnary
             case op of
                 '*' -> flattenedFactor (Mul p p1)
                 '/' -> flattenedFactor (Div p p1))
             <|> return p
             
parseTerm :: Parser Exp
parseTerm = do
    space
    f <- parseFactor
    space
    flattenedTerm f
    where
        flattenedTerm :: Exp -> Parser Exp
        flattenedTerm f = (do
            op <- try $ lexeme (char '+' <|> char '-')
            f1 <- parseFactor
            case op of
                '+' -> flattenedTerm (Add f f1)
                '-' -> flattenedTerm (Sub f f1))
            <|> return f
            
parseComparison :: Parser Exp
parseComparison = do
    space
    t <- parseTerm
    space
    flattenedComparison t
    where
        flattenedComparison :: Exp -> Parser Exp
        flattenedComparison c = (do
            op <- (string ">=" <|> string ">" <|> string "<=" <|> string "<")
            c1 <- parseFactor
            case op of
                ">" -> flattenedComparison (Greater c c1)
                ">=" -> flattenedComparison (GreaterEqual c c1)
                "<" -> flattenedComparison (Less c c1)
                "<=" -> flattenedComparison (LessEqual c c1))
            <|> return c
            
parseEquality :: Parser Exp
parseEquality = do
    space
    c <- parseComparison
    space
    flattenedEquality c
    where
        flattenedEquality :: Exp -> Parser Exp
        flattenedEquality e = (do
            op <- (string "==" <|> string "!=")
            e1 <- parseFactor
            case op of
                "==" -> flattenedEquality (Equal e e1)
                "!=" -> flattenedEquality (NotEqual e e1))
            <|> return e

parseExpression :: Parser Exp
parseExpression = parseEquality

parseExpStmt :: Parser ExpStmt
parseExpStmt = try parseCallFunc <|> (parseExpression >>= return . Expr)

parseCallFunc :: Parser ExpStmt
parseCallFunc = do
    iden <- lexeme (parseIdentifier)
    char '('
    (lexeme (char ')') >> return (CallFunc iden [])) 
     <|> (do
        args <- parseArgs
        char ')'
        return $ CallFunc iden args)
    where
        parseArgs :: Parser [ExpStmt]
        parseArgs = (do
            first <- try parseExpStmt
            rest <- many $ (lexeme $ char ',') >> parseExpStmt
            return (first:rest))
            
parseCallExpStmt :: Parser Stmt
parseCallExpStmt = try (parseCallFunc >>= return . CallExpStmt) 
    <|> (parseExpression >>= return . CallExpStmt . Expr)

parseVarAssign :: Parser Stmt
parseVarAssign = do
    lexeme (string "var")
    iden <- space1 >> parseIdentifier
    lexeme (char '=')
    expStmt <- parseExpStmt
    return (VarAssign iden expStmt)
    
guardError :: Bool -> String -> Parser ()
guardError True s = fail s
guardError False s = return ()
    
parseVarReassign :: Parser Stmt
parseVarReassign = do
    iden <- lexeme (parseIdentifier)
    guardError (iden == "var") "unexpected `var` keyword found in variable reassign"
    lexeme (char '=')
    expStmt <- parseExpStmt
    return (VarReassign iden expStmt)
    
parseIf :: Parser Stmt
parseIf = do
    lexeme (string "if") >> space1
    expStmt <- lexeme parseExpStmt
    stmts <- parseScope
    elses <- elseIfs
    If ((expStmt, stmts) : elses) <$> else'
    where
        elseIfs :: Parser [(ExpStmt, Stmt)]
        elseIfs = many $ do
            lexeme (string "else if") >> space1
            expStmt <- lexeme parseExpStmt
            stmts <- parseScope
            return (expStmt, stmts)
        else' :: Parser Stmt
        else' = try (do
            lexeme (string "else") >> space1
            parseScope) <|> return (Seq [])
    
parseWhile :: Parser Stmt
parseWhile = do
    lexeme (string "while")
    expStmt <- lexeme (parseExpStmt)
    stmts <- parseScope
    return (While expStmt stmts)
    
parseScope :: Parser Stmt
parseScope = do
    lexeme (char '{')
    space
    stmts <- many parseStmt
    lexeme (char '}')
    space
    return (Seq stmts)
    
parseFuncDef :: Parser Stmt
parseFuncDef = do
    lexeme (string "func")
    iden <- lexeme (parseIdentifier)
    char '('
    (lexeme (char ')') >> parseScope >>= (return . FuncDef iden []))
     <|> (do
        args <- parseArgs
        lexeme (char ')')
        stmts <- parseScope
        return $ FuncDef iden args stmts)
    where
        parseArgs :: Parser [String]
        parseArgs = (do
            first <- try parseIdentifier
            rest <- many $ (lexeme $ char ',') >> lexeme (parseIdentifier)
            return (first:rest))
            
parseReturnStmt :: Parser Stmt
parseReturnStmt = lexeme (string "return") >> parseExpStmt >>= return . ReturnStmt

parsePrint :: Parser Stmt
parsePrint = lexeme (string "print") >> parseExpStmt >>= return . Print
 
--data Stmt = VarAssign String ExpStmt
--    | VarReassign String ExpStmt
--    | While ExpStmt Stmt
--    | Seq [Stmt]
--    | FuncDef String [String] Stmt
--    | ReturnStmt ExpStmt
--    | CallExpStmt ExpStmt
--    | Print ExpStmt
--    deriving Show

parseStmt :: Parser Stmt
parseStmt = try parseVarAssign 
    <|> try parseVarReassign 
    <|> try parseWhile 
    <|> try parseFuncDef 
    <|> try parseIf
    <|> try parsePrint 
    <|> try parseReturnStmt 
    <|> parseCallExpStmt

parseProgram :: Parser Stmt
parseProgram = do
    stmts <- many (try parseStmt)
    space
    eof
    return (Seq stmts)
    
--testVarAssign = "func cube(x) {\nreturn x * x * x\n}\nvar y = cube(x)"
--testCallFunc = "hello( )"
--testVarAssign = "var y = 5"

testCallFunc = "var y = cube(x)"

testThing = "func factorial(x) {\nvar total = 1\nwhile x > 1 {} }"

testOtherThing = "while true { func fac() {} }"
    

--parseExpStmt :

--parseStmt :: Parser Stmt
--pasre

exampleText = "!=true"

--parseEquality :: Parser Exp
--parseEquality =


--parsePrimary :: Parser Primary
--parsePrimary = parseFloat <|> parseString <|> parseIdentifier

--parseToken :: Parser TokenType
--parseToken = do
--    space
--    (try parseFloat <|> parseSymbol <|> parseKeyword <|> parseString <|> parseIdentifier)
    
--parsePrimary :: Parser Primary
--parsePrimary = do
--    (f) <- char (T.LEFT_BRACE)
--    return $ FLOAT 432.2
    
--data Ast =