module Lexer (
    TokenType (..),
    getAllTokens
) where
    
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
    
data TokenType = LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | DIVIDE
    | TIMES
    | SEMICOLON
    
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    
    | IDENTIFIER String
    | STRING String
    | FLOAT Float
    
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NULL
    | OR
    | PRINT
    | RETURN
    | TRUE
    | VAR
    | WHILE
    deriving Show
    
type Parser = Parsec Void String

parseSymbol :: Parser TokenType
parseSymbol = (char '(' >> return LEFT_PAREN)
    <|> (char ')' >> return RIGHT_PAREN)
    <|> (char '{' >> return LEFT_BRACE)
    <|> (char '}' >> return RIGHT_BRACE)
    <|> (char ',' >> return COMMA)
    <|> (char '.' >> return DOT)
    <|> (char '-' >> return MINUS)
    <|> (char '+' >> return PLUS)
    <|> (char ';' >> return SEMICOLON)
    <|> (char '!' >> 
        (char '=' >> return BANG_EQUAL) 
        <|> return BANG)
    <|> (char '=' >> 
        (char '=' >> return EQUAL_EQUAL) 
        <|> return EQUAL)
    <|> (char '>' >> 
        (char '=' >> return GREATER_EQUAL) 
        <|> return GREATER)
    <|> (char '<' >> 
        (char '=' >> return LESS_EQUAL) 
        <|> return LESS)
        
parseKeyword :: Parser TokenType
parseKeyword = string "and" >> return AND
    <|> (string "class" >> return CLASS)
    <|> (string "else" >> return ELSE)
    <|> (string "false" >> return FALSE)
    <|> (string "fun" >> return FUN)
    <|> (string "for" >> return FOR)
    <|> (string "if" >> return IF)
    <|> (string "null" >> return NULL)
    <|> (string "or" >> return OR)
    <|> (string "print" >> return PRINT)
    <|> (string "return" >> return RETURN)
    <|> (string "true" >> return TRUE)
    <|> (string "var" >> return VAR)
    <|> (string "while" >> return WHILE)
    
floatNum :: Parser Float
floatNum = do
    int <- some digitChar
    (do 
        char '.'
        float <- some digitChar
        return $ read $ int ++ "." ++ float)
     <|> (return $ read int)
    
parseFloat :: Parser TokenType
parseFloat = (char '-' >> floatNum >>= return . FLOAT . negate) 
    <|> (floatNum >>= return . FLOAT)
     
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

parseString :: Parser TokenType
parseString = stringLiteral >>= return . STRING

parseIdentifier :: Parser TokenType
parseIdentifier = some letterChar >>= return . IDENTIFIER

parseToken :: Parser TokenType
parseToken = do
    space
    (try parseFloat <|> parseSymbol <|> parseKeyword <|> parseString <|> parseIdentifier)

parseTokens :: Parser [TokenType]
parseTokens = do
    tokens <- many parseToken
    eof
    return tokens
    
getAllTokens :: String -> Either String [TokenType]
getAllTokens s = case parse parseTokens "error" s of
    (Left err) -> Left "error lexing"
    (Right tokens) -> Right tokens

exampleText :: String
exampleText = "and \"hello lfjs"