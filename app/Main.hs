module Main where
    
import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Interpret
import Parser
import Ast
import Text.Megaparsec (parseTest, parse)

main :: IO ()
main = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    let thingToDo = args !! 1
    text <- hGetContents file
    case thingToDo of
        "1" -> parseTest parseProgram text
        "2" -> case parse parseProgram "error" text of
            Left err -> putStrLn $ errorBundlePretty err
            Right stmt -> execNew stmt
    