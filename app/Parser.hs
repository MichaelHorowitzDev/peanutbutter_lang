module Parser (
 parseProgram
) where
    
--import qualified Lexer as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad
import Data.Functor
import Ast
    
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = (space >>)
    
lexeme1 :: Parser a -> Parser a
lexeme1 = (space1 >>)

parseNum :: Parser Exp
parseNum = do
    char '-'
    int <- intPart
    (floatPart >>= \float -> return $ Lit $ Float $ negate (read $ int ++ "." ++ float)) 
        <|> return (Lit $ Int $ negate (read int))
    <|> do
        int <- intPart
        (floatPart >>= \float -> return $ Lit $ Float (read $ int ++ "." ++ float))
            <|> return (Lit $ Int (read int))
    where
        intPart :: Parser String
        intPart = do
            int <- some digitChar
            notFollowedBy letterChar
            return int
        floatPart :: Parser String
        floatPart = do
            char '.'
            float <- some digitChar
            notFollowedBy letterChar
            return float

parseString :: Parser String
parseString = char '\"' *> manyTill L.charLiteral (char '\"')

parseIdentifier :: Parser String
parseIdentifier = some letterChar

parseBool :: Parser Bool
parseBool = (string "true" >> return True) <|> (string "false" >> return False)

parsePrimary :: Parser Exp
parsePrimary = space >> (parseString <&> Lit . String) 
    <|> (parseBool <&> Lit . Bool)
    <|> parseNum
    <|> (parseIdentifier <&> Var)
    <|> (do
        char '('
        expr <- parseExpression
        char ')'
        return expr)

parseCallFunc :: Parser Exp
parseCallFunc = do
    primary <- parsePrimary
    flattenedCalls primary
    where
        call :: Parser [Exp]
        call = do
            char '('
            args <- parseArgs
            char ')'
            return args
        parseArgs :: Parser [Exp]
        parseArgs = (do
            first <- try parseExp
            rest <- many $ lexeme (char ',') >> parseExp
            return (first:rest)) <|> return []
        flattenedCalls :: Exp -> Parser Exp
        flattenedCalls exp = (call >>= flattenedCalls . CallFunc exp) <|> return exp
        
parseUnary :: Parser Exp
parseUnary = do
    op <- try $ lexeme $ (char '-' $> Negate) <|> (char '!' $> Bang)
    op <$> parseUnary
    <|> parseCallFunc

parseFactor :: Parser Exp
parseFactor = do
    space
    u <- parseUnary
    space
    flattenedFactor u
     where
         flattenedFactor :: Exp -> Parser Exp
         flattenedFactor p = do
            op <- try $ lexeme $ (char '*' $> Mul) <|> (char '/' $> Div)
            p1 <- parseUnary
            flattenedFactor (op p p1)
            <|> return p
             
parseTerm :: Parser Exp
parseTerm = do
    space
    f <- parseFactor
    space
    flattenedTerm f
    where
        flattenedTerm :: Exp -> Parser Exp
        flattenedTerm f = do
            op <- try $ lexeme $ (char '+' $> Add) <|> (char '-' $> Sub)
            f1 <- parseFactor
            flattenedTerm (op f f1)
            <|> return f
            
parseComparison :: Parser Exp
parseComparison = do
    space
    t <- parseTerm
    space
    flattenedComparison t
    where
        flattenedComparison :: Exp -> Parser Exp
        flattenedComparison t = do
            op <- try $ lexeme $ (string ">=" $> GreaterEqual) 
                <|> (string ">" $> Greater) 
                <|> (string "<=" $> LessEqual) 
                <|> (string "<" $> Less)
            t1 <- parseTerm
            flattenedComparison (op t t1)
            <|> return t
            
parseEquality :: Parser Exp
parseEquality = do
    space
    c <- parseComparison
    space
    flattenedEquality c
    where
        flattenedEquality :: Exp -> Parser Exp
        flattenedEquality e = do
            op <- try $ lexeme $ (string "==" $> Equal) <|> (string "!=" $> NotEqual)
            e1 <- parseComparison
            flattenedEquality (op e e1)
            <|> return e

parseExpression :: Parser Exp
parseExpression = parseEquality

parseExp :: Parser Exp
parseExp = parseEquality

parseCallExp :: Parser Stmt
parseCallExp = CallExp <$> parseExpression

parseVarAssign :: Parser Stmt
parseVarAssign = do
    string "var"
    space1 <|> fail "expected assignment"
    iden <- parseIdentifier <|> fail "no valid identifier found"
    space
    char '=' <|> fail "no `=` found in variable assignment"
    space
    VarAssign iden <$> parseExp <|> fail "no right hand side of equation"
    
guardError :: Bool -> String -> Parser ()
guardError True s = fail s
guardError False s = return ()
    
parseVarReassign :: Parser Stmt
parseVarReassign = do
    iden <- parseIdentifier
    guardError (iden == "var") "unexpected `var` keyword found in variable reassign"
    lexeme (char '=')
    VarReassign iden <$> parseExp
    
parseIf :: Parser Stmt
parseIf = do
    lexeme (string "if") >> space1
    expStmt <- lexeme parseExp
    stmts <- parseScope
    elses <- elseIfs
    If ((expStmt, stmts) : elses) <$> else'
    where
        elseIfs :: Parser [(Exp, Stmt)]
        elseIfs = many $ do
            lexeme (string "else if") >> space1
            expStmt <- lexeme parseExp
            stmts <- parseScope
            return (expStmt, stmts)
        else' :: Parser Stmt
        else' = try (do
            lexeme (string "else") >> space1
            parseScope) <|> return (Seq [])
    
parseWhile :: Parser Stmt
parseWhile = do
    string "while"
    expStmt <- lexeme parseExp
    While expStmt <$> parseScope
    
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
    string "func"
    iden <- lexeme parseIdentifier
    char '('
    (lexeme (char ')') >> (FuncDef iden [] <$> parseScope))
     <|> do
        args <- parseArgs
        lexeme (char ')')
        FuncDef iden args <$> parseScope
    where
        parseArgs :: Parser [String]
        parseArgs = do
            first <- try parseIdentifier
            rest <- many $ lexeme (char ',') >> lexeme parseIdentifier
            return (first:rest)
            
parseReturnStmt :: Parser Stmt
parseReturnStmt = (string "return" >> parseExp) <&> ReturnStmt

parsePrint :: Parser Stmt
parsePrint = (string "print" >> parseExp) <&> Print

parseStmt :: Parser Stmt
parseStmt = parseVarAssign
    <|> parseWhile 
    <|> parseFuncDef
    <|> parseIf
    <|> parsePrint 
    <|> parseReturnStmt 
    <|> try parseVarReassign
    <|> parseCallExp

parseProgram :: Parser Stmt
parseProgram = do
    stmts <- many (space >> parseStmt)
    space
    eof
    return (Seq stmts)

testCallFunc = "var y = cube(x)"

testThing = "func factorial(x) {\nvar total = 1\nwhile x > 1 {} }"

testOtherThing = "while true { func fac() {} }"

exampleText = "!=true"
