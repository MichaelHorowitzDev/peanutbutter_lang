module Main where
    
import System.Environment (getArgs)
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Interpret
import Parser
import Ast
import Text.Megaparsec
import InterpretError

import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Except

makePosState :: String -> a -> PosState a
makePosState filename s = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos filename
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }
    
toParseError :: InterpretError -> PosState String -> ParseErrorBundle String InterpretError
toParseError err state = ParseErrorBundle
    { bundleErrors = NE.singleton (FancyError ((posOffset $ offset err)) $ S.singleton (ErrorCustom err))
    , bundlePosState = state
    }

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadMode
    let thingToDo = args !! 1
    text <- hGetContents file
    case thingToDo of
        "1" -> parseTest parseProgram text
        "2" -> case parse parseProgram filename text of
            Left err -> putStrLn $ errorBundlePretty err
            Right stmt -> do
                result <- runExceptT $ execNew stmt
                case result of
                    Right () -> return ()
                    Left err -> putStr $ errorBundlePretty $ toParseError err (makePosState filename text)
    