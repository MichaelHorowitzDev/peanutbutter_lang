module Main where
    
import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Interpret
import Parser
import Ast
import Text.Megaparsec
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Except

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode
    let thingToDo = args !! 1
    text <- hGetContents file
    case thingToDo of
        "1" -> parseTest parseProgram text
        "2" -> case parse parseProgram "error" text of
            Left err -> putStrLn $ errorBundlePretty err
            Right stmt -> do
                result <- runExceptT $ execNew stmt
                case result of
                    Right () -> return ()
                    Left err -> putStr $ errorBundlePretty $ toParseError' err (makePosState filename text)
    