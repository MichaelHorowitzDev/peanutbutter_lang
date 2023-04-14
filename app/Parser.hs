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
import qualified Data.Vector as V

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = (space >>)

lexeme1 :: Parser a -> Parser a
lexeme1 = (space1 >>)

parseNum :: Parser Exp
parseNum = do
    offset <- getOffset
    num <- do
        char '-'
        int <- intPart
        (floatPart >>= \float -> return $ Lit $ Float $ negate (read $ int ++ "." ++ float))
            <|> return (Lit $ Int $ negate (read int))
        <|> do
            int <- intPart
            (floatPart >>= \float -> return $ Lit $ Float (read $ int ++ "." ++ float))
                <|> return (Lit $ Int (read int))
    len <- getOffset <&> subtract offset
    return $ num (Position offset len)
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
parseIdentifier = do
    first <- letterChar
    rest <- many (letterChar <|> digitChar <|> char '_')
    return (first:rest)

parseBool :: Parser Bool
parseBool = (string "true" >> return True) <|> (string "false" >> return False)

parseBoolLit :: Parser Exp
parseBoolLit = do
    offset <- getOffset
    b <- parseBool
    len <- getOffset <&> subtract offset
    return $ Lit (Bool b) (Position offset len)

parseStringLit :: Parser Exp
parseStringLit = do
    offset <- getOffset
    s <- parseString
    len <- getOffset <&> subtract offset
    return $ Lit (String s) (Position offset len)

parseVar :: Parser Exp
parseVar = do
    offset <- getOffset
    var <- parseIdentifier
    len <- getOffset <&> subtract offset
    return $ Var var (Position offset len)

parseArray :: Parser Exp
parseArray = do
    offset <- getOffset
    char '['
    elems <- parseElems
    char ']'
    len <- getOffset <&> subtract offset
    return $ ArrayDef elems (Position offset len)
    where
        parseElems :: Parser [Exp]
        parseElems = (do
            first <- try parseExp
            rest <- many $ lexeme (char ',') >> parseExp
            return (first:rest)) <|> return []

parsePrimary :: Parser Exp
parsePrimary = space >> parseArray
    <|> parseStringLit
    <|> parseBoolLit
    <|> parseNum
    <|> parseVar
    <|> (do
        char '('
        expr <- parseExp
        char ')'
        return expr)

funcCall :: Parser [Exp]
funcCall = do
    char '('
    space
    args <- parseArgs
    space
    char ')'
    return args
    where
        parseArgs :: Parser [Exp]
        parseArgs = (do
            first <- try parseExp
            space
            rest <- many $ do
                char ','
                exp <- parseExp
                space
                return exp
            return (first:rest)) <|> return []

subscript :: Parser Exp
subscript = do
    char '['
    space
    exp <- try parseExp <|> fail "you forgot to pass in an integer for subscripting"
    space
    char ']'
    return exp

getter :: Parser String
getter = do
    offset <- getOffset
    char '.'
    space
    iden <- parseIdentifier
    len <- getOffset <&> subtract offset
    return iden

parseCallFunc :: Parser Exp
parseCallFunc = do
    offset <- getOffset
    primary <- parsePrimary
    space
    flattenedCalls primary
    where
        flattenedCalls :: Exp -> Parser Exp
        flattenedCalls exp = (do
            offset <- getOffset
            op <- (funcCall <&> CallFunc exp) <|> (subscript <&> Subscript exp) <|> (getter <&> Getter exp)
            len <- getOffset <&> subtract offset
            space
            flattenedCalls $ op (Position offset len)
            ) <|> return exp

parseUnary :: Parser Exp
parseUnary = do
    offset <- getOffset
    op <- try $ lexeme $ (char '-' $> Negate) <|> (char '!' $> Bang)
    u <- parseUnary
    len <- getOffset <&> subtract offset
    return $ op u (Position offset len)
    <|> parseCallFunc

parseFactor :: Parser Exp
parseFactor = do
    space
    offset <- getOffset
    u <- parseUnary
    flattenedFactor offset u
     where
         flattenedFactor :: Int -> Exp -> Parser Exp
         flattenedFactor offset p = do
            op <- try $ lexeme $ (char '*' $> Mul) <|> (char '/' $> Div)
            p1 <- parseUnary
            len <- getOffset <&> subtract offset
            flattenedFactor offset (op p p1 (Position offset len))
            <|> return p

parseTerm :: Parser Exp
parseTerm = do
    space
    offset <- getOffset
    f <- parseFactor
    flattenedTerm offset f
    where
        flattenedTerm :: Int -> Exp -> Parser Exp
        flattenedTerm offset f = do
            op <- try $ lexeme $ (char '+' $> Add) <|> (char '-' $> Sub)
            f1 <- parseFactor
            len <- getOffset <&> subtract offset
            flattenedTerm offset (op f f1 (Position offset len))
            <|> return f

parseComparison :: Parser Exp
parseComparison = do
    space
    offset <- getOffset
    t <- parseTerm
    (do
        op <- try $ lexeme $ (string ">=" $> GreaterEqual)
                <|> (string ">" $> Greater)
                <|> (string "<=" $> LessEqual)
                <|> (string "<" $> Less)
        t1 <- parseTerm
        len <- getOffset <&> subtract offset
        return (op t t1 (Position offset len))) <|> return t

parseEquality :: Parser Exp
parseEquality = do
    space
    offset <- getOffset
    c <- parseComparison
    (do
        op <- try $ lexeme $ (string "==" $> Equal) <|> (string "!=" $> NotEqual)
        e1 <- parseComparison
        len <- getOffset <&> subtract offset
        return (op c e1 (Position offset len))) <|> return c

parseLambda :: Parser Exp
parseLambda = (do
    offset <- getOffset
    char '\\' <|> char 'λ'
    params <- parseParams
    space
    string "->"
    exp <- parseExp
    len <- getOffset <&> subtract offset
    return $ Lambda params exp (Position offset len)) <|> parseEquality
    where
        parseParams :: Parser [String]
        parseParams = do
            first <- space >> try parseIdentifier
            try (do
                rest <- many (try $ space1 >> parseIdentifier)
                return $ first:rest) <|> return [first]

parseExp :: Parser Exp
parseExp = parseLambda

parseCallExp :: Parser (Position -> Stmt)
parseCallExp = CallExp <$> parseExp

keyword :: String -> Parser ()
keyword s = try (string s >> notFollowedBy alphaNumChar)

parseVarAssign :: Parser (Position -> Stmt)
parseVarAssign = do
    keyword "var"
    space1 <|> fail "expected assignment"
    iden <- parseIdentifier <|> fail "no valid identifier found"
    space
    char '=' <|> fail "no `=` found in variable assignment"
    space
    VarAssign iden <$> parseExp <|> fail "no right hand side of equation"

parseLetAssign :: Parser (Position -> Stmt)
parseLetAssign = do
    keyword "let"
    space1 <|> fail "expected assignment"
    iden <- parseIdentifier <|> fail "no valid identifier found"
    space
    char '=' <|> fail "no `=` found in variable assignment"
    space
    LetAssign iden <$> parseExp <|> fail "no right hand side of equation"

guardError :: Bool -> String -> Parser ()
guardError True s = fail s
guardError False s = return ()

parseVarReassign :: Parser (Position -> Stmt)
parseVarReassign = do
    iden <- parseIdentifier
    guardError (iden == "var") "unexpected `var` keyword found in variable reassign"
    lexeme (char '=')
    VarReassign iden <$> parseExp

parseIf :: Parser (Position -> Stmt)
parseIf = do
    keyword "if" >> space1
    expStmt <- lexeme parseExp
    stmts <- parseScope
    elses <- elseIfs
    If ((expStmt, stmts) : elses) <$> else'
    where
        elseIfs :: Parser [(Exp, [Stmt])]
        elseIfs = many $ do
            lexeme (string "else if") >> space1
            expStmt <- lexeme parseExp
            stmts <- parseScope
            return (expStmt, stmts)
        else' :: Parser [Stmt]
        else' = try (do
            lexeme (string "else") >> space1
            parseScope) <|> return []

parseWhile :: Parser (Position -> Stmt)
parseWhile = do
    keyword "while"
    space1
    expStmt <- lexeme parseExp
    While expStmt <$> parseScope

parseScope :: Parser [Stmt]
parseScope = do
    lexeme (char '{')
    space
    stmts <- many (parseStmt >>= \stmt -> space >> return stmt)
    space
    lexeme (char '}')
    space
    return stmts

parseFuncDef :: Parser (Position -> Stmt)
parseFuncDef = do
    keyword "func"
    space1
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

parseDataDef :: Parser (Position -> Stmt)
parseDataDef = do
    keyword "data"
    space1
    iden <- lexeme parseIdentifier
    args <- parseArgs
    DataDef iden args <$> parseScope
    where
        parseArgs :: Parser [String]
        parseArgs = (do
            char '('
            args <- (do
                first <- parseIdentifier
                rest <- many $ lexeme (char ',') >> lexeme parseIdentifier
                return (first:rest)
                ) <|> return []
            char ')'
            return args
            )
            <|> return []

parseReturnStmt :: Parser (Position -> Stmt)
parseReturnStmt = (keyword "return" >> space1 >> parseExp) <&> ReturnStmt

parsePrint :: Parser (Position -> Stmt)
parsePrint = (keyword "print" >> space1 >> parseExp) <&> Print

semicolonStmt :: Parser (Position -> Stmt)
semicolonStmt = do
    stmt <- parseVarAssign
        <|> parseLetAssign
        <|> parsePrint
        <|> parseReturnStmt
        <|> try parseVarReassign
        <|> parseCallExp
    char ';'
    return stmt

parseStmt :: Parser Stmt
parseStmt = do
    offset <- getOffset
    stmt <- parseWhile
        <|> parseFuncDef
        <|> parseDataDef
        <|> parseIf
        <|> semicolonStmt
    len <- getOffset <&> subtract offset
    return $ stmt (Position offset len)

parseProgram :: Parser [Stmt]
parseProgram = do
    space
    stmts <- many (parseStmt >>= \stmt -> space >> return stmt)
    space
    eof
    return stmts
