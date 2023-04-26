module Parser (
 parseProgram
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad
import Data.Functor
import Ast
import qualified Data.Vector as V
import Data.Char (digitToInt)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    empty

parseNum :: Parser Exp
parseNum = binary <|> hex <|> oct <|> normalNum
    where
        int :: Parser Int
        int = do
            int <- some digitChar
            notFollowedBy letterChar
            return $ read int
        intPart :: Parser Int
        intPart = do
            char '-'
            negate <$> int
            <|> int
        doublePart :: Int -> Parser Double
        doublePart int = do
            char '.'
            float <- some digitChar
            notFollowedBy letterChar
            return $ read (show int ++ "." ++ float)
        normalNum :: Parser Exp
        normalNum = do
            offset <- getOffset
            int <- intPart
            num <- (Lit . Double <$> doublePart int) <|> return (Lit $ Int int)
            len <- getOffset <&> subtract offset
            return $ num (Position offset len)
        base :: Int -> Char -> String -> Parser Char -> Parser Exp
        base b letter name p = do
            offset <- getOffset
            string ['0', letter]
            list <- map digitToInt <$> some p
            notFollowedBy numberChar
                <|> (lookAhead numberChar >>= \c -> fail ("invalid digit '" ++ [c] ++ "' in " ++ name ++ " number"))
            notFollowedBy letterChar
                <|> (lookAhead letterChar >>= \c -> fail ("invalid character '" ++ [c] ++ "' in " ++ name ++ " number"))
            let n = foldl (\acc x -> acc * b + x) 0 list
            len <- getOffset <&> subtract offset
            return $ (Lit $ Int n) (Position offset len)

        binary = base 2 'b' "binary" binDigitChar
        hex = base 16 'x' "hex" hexDigitChar
        oct = base 8 'o' "octal" octDigitChar

parseString :: Parser String
parseString = char '\"' >> manyTill L.charLiteral (char '\"')

parseIdentifier :: Parser String
parseIdentifier = lexeme $ do
    first <- letterChar
    rest <- many (letterChar <|> digitChar <|> char '_')
    return (first:rest)

parseBool :: Parser Bool
parseBool = (keyword "true" $> True) <|> (keyword "false" $> False)

parseWithPos :: Parser (Position -> a) -> Parser a
parseWithPos p = do
    offset <- getOffset
    x <- p
    len <- getOffset <&> subtract offset
    return $ x (Position offset len)

parseBoolLit :: Parser Exp
parseBoolLit = parseWithPos (parseBool <&> Lit . Bool)

parseStringLit :: Parser Exp
parseStringLit = parseWithPos (parseString <&> Lit . String)

parseVar :: Parser Exp
parseVar = parseWithPos (parseIdentifier <&> Var)

parseArray :: Parser Exp
parseArray = parseWithPos $ do
    lexeme (char '[')
    elems <- parseElems
    lexeme (char ']')
    return $ ArrayDef elems
    where
        parseElems :: Parser [Exp]
        parseElems = (do
            offset <- getOffset
            start <- lexeme int
            (do
                lexeme (string "..")
                end <- lexeme int <|> fail "expected upper bound for range"
                len <- getOffset <&> subtract offset
                return $ map (\x -> (Lit $ Int x) (Position offset len)) [start..end])
                <|> do
                    len <- getOffset <&> subtract offset
                    rest ((Lit $ Int start) (Position offset len))
            ) <|> (do
            first <- try parseExp
            rest <- many $ lexeme (char ',') >> parseExp
            return (first:rest)) <|> return []
            where
                rest :: Exp -> Parser [Exp]
                rest first = do
                    rest <- many $ lexeme (char ',') >> parseExp
                    return (first:rest)

nat :: Parser Int
nat = do
    int <- some digitChar
    notFollowedBy letterChar
    return $ read int
int :: Parser Int
int = do
    char '-'
    negate <$> nat
    <|> nat

parsePrimary :: Parser Exp
parsePrimary = lexeme (parseArray
        <|> parseStringLit
        <|> parseBoolLit
        <|> parseNum
        <|> parseVar
        <|> (do
            lexeme (char '(')
            expr <- parseExp
            lexeme (char ')')
            return expr))

funcCall :: Parser [Exp]
funcCall = do
    lexeme (char '(')
    args <- lexeme parseArgs
    char ')'
    return args
    where
        parseArgs :: Parser [Exp]
        parseArgs = (do
            first <- parseExp
            rest <- many $ do
                lexeme (char ',')
                parseExp
            return (first:rest)) <|> return []

subscriptSlice :: Exp -> Parser (Position -> Exp)
subscriptSlice exp = do
    lexeme (char '[')
    notFollowedBy (char ']') <|> fail "you forgot to pass in an integer for subscripting"
    posExp <- (do
        exp' <- parseExp
        (latter <&> Slice exp (Just exp')) <|> return (Subscript exp exp')
        ) <|> (latter <&> Slice exp Nothing)
    char ']'
    return posExp
    where
        latter :: Parser (Maybe Exp)
        latter = do
            lexeme (char ':')
            optional parseExp

getter :: Parser String
getter = do
    offset <- getOffset
    lexeme (char '.')
    iden <- parseIdentifier
    len <- getOffset <&> subtract offset
    return iden

parseCallFunc :: Parser Exp
parseCallFunc = do
    primary <- parsePrimary
    flattenedCalls primary
    where
        flattenedCalls :: Exp -> Parser Exp
        flattenedCalls exp = (do
            offset <- getOffset
            op <- (funcCall <&> CallFunc exp) <|> subscriptSlice exp <|> (getter <&> Getter exp)
            len <- getOffset <&> subtract offset
            sc
            flattenedCalls $ op (Position offset len)
            ) <|> return exp

parseUnary :: Parser Exp
parseUnary = do
    offset <- getOffset
    op <- lexeme ((char '-' $> Negate) <|> (char '!' $> Bang))
    u <- parseUnary
    len <- getOffset <&> subtract offset
    return $ op u (Position offset len)
    <|> parseCallFunc

parseFactor :: Parser Exp
parseFactor = do
    offset <- getOffset
    u <- parseUnary
    flattenedFactor offset u
    where
        flattenedFactor :: Int -> Exp -> Parser Exp
        flattenedFactor offset p = do
            op <- lexeme ((char '*' $> Mul) <|> (char '/' $> Div))
            p1 <- parseUnary
            len <- getOffset <&> subtract offset
            flattenedFactor offset (op p p1 (Position offset len))
            <|> return p

parseTerm :: Parser Exp
parseTerm = do
    offset <- getOffset
    f <- parseFactor
    flattenedTerm offset f
    where
        flattenedTerm :: Int -> Exp -> Parser Exp
        flattenedTerm offset f = do
            op <- lexeme ((char '+' $> Add) <|> (char '-' $> Sub))
            f1 <- parseFactor
            len <- getOffset <&> subtract offset
            flattenedTerm offset (op f f1 (Position offset len))
            <|> return f

parseComparison :: Parser Exp
parseComparison = do
    offset <- getOffset
    t <- parseTerm
    (do
        op <- lexeme ((string ">=" $> GreaterEqual)
                <|> (string ">" $> Greater)
                <|> (string "<=" $> LessEqual)
                <|> (string "<" $> Less))
        t1 <- parseTerm
        len <- getOffset <&> subtract offset
        return (op t t1 (Position offset len))) <|> return t

parseEquality :: Parser Exp
parseEquality = do
    offset <- getOffset
    c <- parseComparison
    (do
        op <- lexeme (string "==" $> Equal) <|> (string "!=" $> NotEqual)
        e1 <- parseComparison
        len <- getOffset <&> subtract offset
        return (op c e1 (Position offset len))) <|> return c

parseBoolOp :: Parser Exp
parseBoolOp = do
    offset <- getOffset
    e <- parseEquality
    flattenedBool offset e
    where
        flattenedBool :: Int -> Exp -> Parser Exp
        flattenedBool offset e = do
            op <- lexeme ((string "&&" $> And) <|> (string "||" $> Or))
            e1 <- parseEquality
            len <- getOffset <&> subtract offset
            flattenedBool offset (op e e1 (Position offset len))
            <|> return e

parseLambda :: Parser Exp
parseLambda = (do
    offset <- getOffset
    lexeme (char '\\' <|> char 'λ')
    params <- parseParams
    lexeme (string "->")
    exp <- parseExp
    len <- getOffset <&> subtract offset
    return $ Lambda params exp (Position offset len)) <|> parseBoolOp
    where
        parseParams :: Parser [String]
        parseParams = do
            first <- parseIdentifier
            (do
                rest <- many parseIdentifier
                return $ first:rest)

parseExp :: Parser Exp
parseExp = parseLambda

parseCallExp :: Parser (Position -> Stmt)
parseCallExp = CallExp <$> parseExp

keyword :: String -> Parser String
keyword s = lexeme $ try (string s <* notFollowedBy alphaNumChar)

parseVarAssign :: Parser (Position -> Stmt)
parseVarAssign = do
    keyword "var"
    iden <- parseIdentifier <|> fail "no valid identifier found"
    lexeme (char '=') <|> fail "no `=` found in variable assignment"
    VarAssign iden <$> parseExp <|> fail "no right hand side of equation"

parseLetAssign :: Parser (Position -> Stmt)
parseLetAssign = do
    keyword "let"
    iden <- parseIdentifier <|> fail "no valid identifier found"
    lexeme (char '=') <|> fail "no `=` found in variable assignment"
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
    keyword "if"
    expStmt <- lexeme parseExp
    stmts <- parseScope
    elses <- elseIfs
    If ((expStmt, stmts) : elses) <$> else'
    where
        elseIfs :: Parser [(Exp, [Stmt])]
        elseIfs = many $ do
            keyword "else if"
            expStmt <- lexeme parseExp
            stmts <- parseScope
            return (expStmt, stmts)
        else' :: Parser [Stmt]
        else' = (do
            keyword "else"
            parseScope) <|> return []

parseWhile :: Parser (Position -> Stmt)
parseWhile = do
    keyword "while"
    expStmt <- lexeme parseExp
    While expStmt <$> parseScope

parseScope :: Parser [Stmt]
parseScope = do
    lexeme (char '{')
    stmts <- many (lexeme parseStmt)
    lexeme (char '}')
    return stmts

parseFuncDef :: Parser (Position -> Stmt)
parseFuncDef = do
    keyword "func"
    iden <- parseIdentifier
    lexeme (char '(')
    (lexeme (char ')') >> (FuncDef iden [] <$> parseScope))
     <|> do
        args <- parseArgs
        lexeme (char ')')
        FuncDef iden args <$> parseScope
    where
        parseArgs :: Parser [String]
        parseArgs = do
            first <- parseIdentifier
            rest <- many $ lexeme (char ',') >> parseIdentifier
            return (first:rest)

parseDataDef :: Parser (Position -> Stmt)
parseDataDef = do
    keyword "data"
    iden <- parseIdentifier
    args <- parseArgs
    DataDef iden args <$> parseScope
    where
        parseArgs :: Parser [String]
        parseArgs = (do
            lexeme (char '(')
            args <- (do
                first <- parseIdentifier
                rest <- many $ lexeme (char ',') >> parseIdentifier
                return (first:rest)
                ) <|> return []
            lexeme (char ')')
            return args
            )
            <|> return []

parseReturnStmt :: Parser (Position -> Stmt)
parseReturnStmt = (keyword "return" >> parseExp) <&> ReturnStmt

parsePrint :: Parser (Position -> Stmt)
parsePrint = (keyword "print" >> parseExp) <&> Print

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
    stmts <- many (lexeme parseStmt)
    eof
    return stmts
